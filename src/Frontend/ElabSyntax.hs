{-# LANGUAGE FlexibleContexts, FlexibleInstances, BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.ElabSyntax (doElab, Term(..), Program, Cmd(..), elabTests) where

import Values ( Lit(..) )
import Var (Var(..), TyVar, FunVar)
import Control.Monad.State (MonadState (put), get, StateT (runStateT), evalStateT)
import Control.Monad.Except (MonadError (throwError), ExceptT, runExceptT, Except)
import Types

import qualified Frontend.SurfaceSyntax as Surf
import Control.Monad.Identity (Identity (runIdentity))
import Util.PrettyPrint (PrettyPrint(..))
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS.Strict (MonadReader (local, ask))
import Control.Monad.Reader (ReaderT (runReaderT), asks, runReader)
import Test.HUnit
import Data.List (intercalate)
import qualified Data.Functor
import Data.Functor ((<&>))
import qualified HistPgm as Hist
import Debug.Trace (trace)

{-

This module is a general elaboration pass on the surface syntax, taking care of any purely syntactic transformation we need handled.
Note that this does not emit core terms: going to core terms requires type information. This pass handles all the translations that
don't require any type information.

The pass includes:


1. Resolving function calls as either (a) recursive calls, or (b) calls to functions defined earlier in the file.
2. Precomposing elimination forms with cuts to go from the natural deduction surface syntax to the sequent calculus of the elaborated syntax.
For example, we translate (let (a;b) = e1 in e2) to (let z = e1 in (let (a;b) = z in e2)), for fresh z.
3. Renaming variables to eliminate shadowing. This just requires generating fresh a fresh name when we see that a shadow is happening,
and then replacing the shadowed name with the fresh name in the body.

-}

data Term =
      TmLitR Lit
    | TmEpsR
    | TmVar Var
    | TmCatL Var Var Var Term
    | TmCatR Term Term
    | TmParL Var Var Var Term
    | TmParR Term Term
    | TmInl Term
    | TmInr Term
    | TmPlusCase Var Var Term Var Term
    | TmIte Var Term Term
    | TmNil
    | TmCons Term Term
    | TmStarCase Var Term Var Var Term
    | TmCut Var Term Term
    | TmWait Var Term
    | TmHistPgm Hist.Term
    | TmRec (CtxStruct Term)
    | TmFunCall Var.FunVar [TyF Var.TyVar] (CtxStruct Term)
    deriving (Eq, Ord, Show)

instance PrettyPrint Term where
    pp = go False
        where
            go _ (TmLitR l) = pp l
            go _ TmEpsR = "sink"
            go _ (TmVar (Var x)) = x
            go _ TmNil = "nil"
            go _ (TmCatR e1 e2) = concat ["(",go False e1,";",go False e2,")"]
            go _ (TmParR e1 e2) = concat ["(",go False e1,",",go False e2,")"]
            go _ (TmHistPgm eh) = concat ["{",pp eh,"}"]
            go True e = concat ["(", go False e, ")"]
            go False (TmCatL (Var x) (Var y) (Var z) e) = concat ["let (",x,";",y,") = ",z," in ",go False e]
            go False (TmParL (Var x) (Var y) (Var z) e) = concat ["let (",x,",",y,") = ",z," in ",go False e]
            go False (TmIte z e1 e2) = concat ["if ", pp z, " then ", go True e1, " else ", go True e2]
            go False (TmInl e) = "inl " ++ go True e
            go False (TmInr e) = "inr " ++ go True e
            go False (TmPlusCase (Var z) (Var x) e1 (Var y) e2) = concat ["case ",z," of inl ",x," => ",go True e1," | inr",y," => ",go True e2]
            go False (TmCut (Var x) e1 e2) = concat ["let ",x," = ",go True e1," in ",go True e2]
            go False (TmCons e1 e2) = concat [go True e1," :: ", go True e2]
            go False (TmStarCase (Var z) e1 (Var x) (Var xs) e2) = concat ["case ",z," of nil => ",go True e1," | ",x,"::",xs," => ",go True e2]
            go False (TmRec es) = concat ["rec (", pp (go False <$> es), ")"]
            go False (TmFunCall f [] es) = concat [pp f,"(", pp (go False <$> es), ")"]
            go False (TmFunCall f ts es) = concat [pp f,"[",intercalate "," (map pp ts),"]","(", pp (go False <$> es), ")"]
            go False (TmWait x e) = concat ["wait ", pp x," do ", go True e]

data Cmd =
      FunDef Var.FunVar [Var.TyVar] (Ctx Var.Var (TyF Var.TyVar)) (TyF Var.TyVar) Term
    | SpecializeCommand Var.FunVar [Ty]
    | RunCommand Var.FunVar (Ctx Var Surf.UntypedPrefix)
    | RunStepCommand Var.FunVar (Ctx Var Surf.UntypedPrefix)
    deriving (Eq,Ord,Show)

type Program = [Cmd]


data ElabErr =
      UnboundVar Var
    | EqualBoundVars Var
    | PolymorphicRec Var.FunVar
    deriving (Eq, Ord, Show)

instance PrettyPrint ElabErr where
    pp (UnboundVar x) = concat ["Variable ",pp x," not bound. This is a compiler bug."]
    pp (EqualBoundVars x) = concat ["Binding two copies of the same variable ",pp x]
    pp (PolymorphicRec f) = concat ["Polmorphic recursive calls not supported (function ",pp f,")"]

data UnshadowInput = EI {renaming :: M.Map Var Var, funName :: Var.FunVar }

class (MonadState Int m, MonadReader UnshadowInput m, MonadError ElabErr m) => UnshadowM m where


freshVar :: (MonadState Int m) => String -> m Var
freshVar x = do
    n <- get
    put (n + 1)
    return (Var.Var ("_" ++ x ++ show n))

{-
`lowerSurf` mplements phases 1 and 2 of elaboration: lowers the surface language to elaboration language by
resolving recursive calls, inserting cuts, and turning waits of sequences of variables into a sequence of waits.
-}

lowerPattern :: (MonadState Int m) => Maybe Var -> m Var
lowerPattern (Just x) = return x
lowerPattern Nothing = do
    n <- get
    put (n + 1)
    return $ Var.Var $ "_w" ++ show n

freshCutVar :: (MonadState Int m) => m Var
freshCutVar = freshVar "c"


sameBinder :: Maybe Var -> Maybe Var -> Maybe Var
sameBinder Nothing _ = Nothing
sameBinder _ Nothing = Nothing
sameBinder (Just x) (Just y) = if x == y then Just x else Nothing

lowerSurf :: (MonadState Int m, MonadReader Var.FunVar m, MonadError ElabErr m) => Surf.Term -> m Term
lowerSurf (Surf.TmLitR l) = return (TmLitR l)
lowerSurf Surf.TmEpsR = return TmEpsR
lowerSurf (Surf.TmVar x) = return (TmVar x)
lowerSurf (Surf.TmCatL mx my e1 e2) = do
    case sameBinder mx my of
        Just x -> throwError (EqualBoundVars x)
        _ -> return ()
    x <- lowerPattern mx
    y <- lowerPattern my
    e1' <- lowerSurf e1
    e2' <- lowerSurf e2
    z <- freshCutVar
    return $ TmCut z e1' (TmCatL x y z e2')
lowerSurf (Surf.TmCatR e1 e2) = do
    e1' <- lowerSurf e1
    e2' <- lowerSurf e2
    return (TmCatR e1' e2')
lowerSurf (Surf.TmParL mx my e1 e2) = do
    case sameBinder mx my of
        Just x -> throwError (EqualBoundVars x)
        _ -> return ()
    x <- lowerPattern mx
    y <- lowerPattern my
    e1' <- lowerSurf e1
    e2' <- lowerSurf e2
    z <- freshCutVar
    return $ TmCut z e1' (TmParL x y z e2')
lowerSurf (Surf.TmParR e1 e2) = do
    e1' <- lowerSurf e1
    e2' <- lowerSurf e2
    return (TmParR e1' e2')
lowerSurf (Surf.TmInl e) = TmInl <$> lowerSurf e
lowerSurf (Surf.TmInr e) = TmInr <$> lowerSurf e
lowerSurf (Surf.TmIte e e1 e2) = do
    e' <- lowerSurf e
    e1' <- lowerSurf e1
    e2' <- lowerSurf e2
    z <- freshCutVar
    return $ TmCut z e' (TmIte z e1' e2')
lowerSurf (Surf.TmPlusCase e mx e1 my e2) = do
    e' <- lowerSurf e
    x <- lowerPattern mx
    y <- lowerPattern my
    e1'<- lowerSurf e1
    e2'<- lowerSurf e2
    z <- freshCutVar
    return $ TmCut z e' (TmPlusCase z x e1' y e2')
lowerSurf Surf.TmNil = return TmNil
lowerSurf (Surf.TmCons e1 e2) = TmCons <$> lowerSurf e1 <*> lowerSurf e2
lowerSurf (Surf.TmStarCase e e1 mx mxs e2) = do
    e' <- lowerSurf e
    e1' <- lowerSurf e1
    x <- lowerPattern mx
    xs <- lowerPattern mxs
    e2' <- lowerSurf e2
    z <- freshCutVar
    return $ TmCut z e' (TmStarCase z e1' x xs e2')
lowerSurf (Surf.TmWait xs e) = do
    go xs <$> lowerSurf e
    where
        go [] e' = e'
        go (x:xs') e' = TmWait x (go xs' e')
lowerSurf (Surf.TmHistPgm he) = return (TmHistPgm he)
lowerSurf (Surf.TmCut x e1 e2) = do
    e1' <- lowerSurf e1
    e2' <- lowerSurf e2
    return (TmCut x e1' e2')
lowerSurf (Surf.TmFunCall f ts es) = do
    curF <- ask
    if f == curF then 
        if not (null ts) then throwError (PolymorphicRec f) else TmRec <$> mapM lowerSurf es
    else TmFunCall f ts <$> mapM lowerSurf es


freshUnshadowVar :: (UnshadowM m) => m Var
freshUnshadowVar = freshVar "u"


withUnshadow :: (UnshadowM m) => Var -> m a -> m (a,Var)
withUnshadow x u = do
    EI sm _ <- ask
    y <- if M.member x sm then freshUnshadowVar else return x
    a <- local (\(EI sm' fn) -> EI (M.insert x y sm') fn) u
    return (a,y)
    -- if M.member x sm then do
    --     y <- freshUnshadowVar
    --     a <- local (\(EI sm' fn) -> EI (M.insert x y sm') fn) u
    --     return (a,y)
    -- else do
    --     a <- local (\(EI sm' fn) -> EI (M.insert x x sm') fn) u
    --     return (a,x)

unshadowVar :: (UnshadowM m) => Var -> m Var
unshadowVar x = do
    EI sm _ <- ask
    case M.lookup x sm of
        Just y -> return y
        Nothing -> throwError (UnboundVar x)

{-
Carry around a map of variables to variables, mapping the source string to the (known-fresh) variable that it should be replaced with.
This starts with mapping every free variable to itself. Once we notice a variable is shadowed, the map gets updated, mapping the bound
variable to a fresh name.

Every time we encounter a binder, we run `withUnshadow` before checking the continuation.
This rebinds all occurrences of the binder with a fresh variable if the binder was not fresh.
Every use of a variable is then transformed by a call to unshadowVar, which looks up the fresh variable it's mapped ot.
-}
unshadowTerm :: (UnshadowM m) => Term -> m Term
unshadowTerm (TmLitR l) = return (TmLitR l)
unshadowTerm TmEpsR = return TmEpsR
unshadowTerm (TmHistPgm he) = TmHistPgm <$> unshadowHist he
unshadowTerm (TmVar x) = TmVar <$> unshadowVar x
unshadowTerm (TmCatL x y z e) = do
    z' <- unshadowVar z
    ((e',y'),x') <- withUnshadow x $ withUnshadow y $ unshadowTerm e
    return (TmCatL x' y' z' e')
unshadowTerm (TmCatR e1 e2) = do
    e1' <- unshadowTerm e1
    e2' <- unshadowTerm e2
    return (TmCatR e1' e2')
unshadowTerm (TmParL x y z e) = do
    z' <- unshadowVar z
    ((e',y'),x') <- withUnshadow x $ withUnshadow y $ unshadowTerm e
    return (TmParL x' y' z' e')
unshadowTerm (TmParR e1 e2) = do
    e1' <- unshadowTerm e1
    e2' <- unshadowTerm e2
    return (TmParR e1' e2')
unshadowTerm (TmInl e) = TmInl <$> unshadowTerm e
unshadowTerm (TmInr e) = TmInr <$> unshadowTerm e
unshadowTerm (TmIte z e1 e2) = do
    z' <- unshadowVar z
    e1' <- unshadowTerm e1
    e2' <- unshadowTerm e2
    return (TmIte z' e1' e2')
unshadowTerm (TmPlusCase z x e1 y e2) = do
    z' <- unshadowVar z
    (e1',x') <- withUnshadow x $ unshadowTerm e1
    (e2',y') <- withUnshadow y $ unshadowTerm e2
    return (TmPlusCase z' x' e1' y' e2')
unshadowTerm TmNil = return TmNil
unshadowTerm (TmCons e1 e2) = TmCons <$> unshadowTerm e1 <*> unshadowTerm e2
unshadowTerm (TmStarCase z e1 x xs e2) = do
    z' <- unshadowVar z
    e1' <- unshadowTerm e1
    ((e2',xs'),x') <- withUnshadow x $ withUnshadow xs $ unshadowTerm e2
    return (TmStarCase z' e1' x' xs' e2')
unshadowTerm (TmFunCall f ts es) = do
    es' <- mapM unshadowTerm es
    return (TmFunCall f ts es')
unshadowTerm (TmWait x e) = TmWait <$> unshadowVar x <*> unshadowTerm e
unshadowTerm (TmCut x e1 e2) = do
    e1' <- unshadowTerm e1
    (e2',x') <- withUnshadow x (unshadowTerm e2)
    return (TmCut x' e1' e2')
unshadowTerm (TmRec e) = TmRec <$> mapM unshadowTerm e

unshadowHist :: (UnshadowM m) => Hist.Term -> m Hist.Term
unshadowHist (Hist.TmVar x) = Hist.TmVar <$> unshadowVar x
unshadowHist e@(Hist.TmValue _) = return e
unshadowHist e@Hist.TmEps = return e
unshadowHist (Hist.TmBinOp b e1 e2) = Hist.TmBinOp b <$> unshadowHist e1 <*> unshadowHist e2
unshadowHist (Hist.TmMonOp m e) = Hist.TmMonOp m <$> unshadowHist e
unshadowHist (Hist.TmPair e1 e2) = Hist.TmPair <$> unshadowHist e1 <*> unshadowHist e2
unshadowHist (Hist.TmInl e) = Hist.TmInl <$> unshadowHist e
unshadowHist (Hist.TmInr e) = Hist.TmInr <$> unshadowHist e
unshadowHist e@Hist.TmNil = return e
unshadowHist (Hist.TmCons e1 e2) = Hist.TmCons <$> unshadowHist e1 <*> unshadowHist e2
unshadowHist (Hist.TmIte e e1 e2) = Hist.TmIte <$> unshadowHist e <*> unshadowHist e1 <*> unshadowHist e2

-- freshUnshadowVar :: (UnshadowM m) => m Var
-- freshUnshadowVar = freshVar "u"

instance UnshadowM (StateT Int (ReaderT UnshadowInput (Either ElabErr))) where


elabSingle :: Var.FunVar -> Surf.Term -> S.Set Var -> Either ElabErr Term
elabSingle f e s = do
    lowered_e <- runReaderT (evalStateT (lowerSurf e) 0) f
    unshadowed_e <- runReaderT (evalStateT (unshadowTerm lowered_e) 0) initUnshadowMap
    return unshadowed_e
    where
        initUnshadowMap = EI (S.fold (\x -> M.insert x x) M.empty s) f

doElab :: Surf.Program -> IO Program
doElab = mapM $ \case
                    (Surf.FunDef f tvs g s e) ->
                        case elabSingle f e (M.keysSet $ ctxBindings g) of
                            Right e' -> do
                                putStrLn $ "Function " ++ pp f ++ " elaborated OK. Elab term: " ++ pp e' ++ "\n"
                                return (FunDef f tvs g s e')
                            Left err -> error (pp err)
                    (Surf.SpecializeCommand f ts) -> case mapM closeTy ts of
                                                       Left x -> error $ "Tried to specialize function " ++ pp f ++ ", but provided type with type variable " ++ pp x
                                                       Right ts' -> return (SpecializeCommand f ts')
                    (Surf.RunCommand s xs) -> return (RunCommand s xs)
                    (Surf.RunStepCommand s xs) -> return (RunStepCommand s xs)

-- >>> elabSingle (Surf.TmCatL Nothing Nothing (Surf.TmCatR (Surf.TmLitR (LInt 4)) (Surf.TmLitR (LInt 4))) Surf.TmEpsR) (S.fromList [])
-- Right (TmCut (Var "__x2") (TmCatR (TmLitR (LInt 4)) (TmLitR (LInt 4))) (TmCatL (Var "__x0") (Var "__x1") (Var "__x2") TmEpsR),ES {nextVar = 3})

elabTests :: Test
elabTests = TestList [
        elabTest (Surf.TmVar (Var.Var "x")) (TmVar (Var.Var "x")) ["x"],
        elabTest (Surf.TmCatL Nothing Nothing (Surf.TmCatR (Surf.TmLitR (LInt 4)) (Surf.TmLitR (LInt 4))) Surf.TmEpsR) (TmCut (Var "__x2") (TmCatR (TmLitR (LInt 4)) (TmLitR (LInt 4))) (TmCatL (Var "__x0") (Var "__x1") (Var "__x2") TmEpsR)) [],
        elabFails (Surf.TmCatL (Just (Var.Var "y")) (Just (Var.Var "y")) (Surf.TmVar $ Var.Var "x") Surf.TmEpsR) ["x"],
        elabTest (Surf.TmCatL (Just (Var.Var "y")) (Just (Var.Var "z")) (Surf.TmVar (Var.Var "z")) (Surf.TmVar (Var.Var "z"))) (TmCut (Var "__x1") (TmVar (Var "z")) (TmCatL (Var "y") (Var "__x0") (Var "__x1") (TmVar (Var "__x0")))) ["z"],
        elabFails (Surf.TmCatL (Just (Var.Var "y")) (Just (Var.Var "z")) (Surf.TmVar (Var.Var "z")) (Surf.TmVar (Var.Var "z"))) []
    ]
    where
        elabTest e e'' xs = TestCase $ do
            case elabSingle undefined e (S.fromList $ Var.Var <$> xs) of
                Right e' -> assertEqual "" e' e''
                Left err -> assertFailure (pp err)
        elabFails e xs = TestCase $ do
            case elabSingle undefined e (S.fromList $ Var.Var <$> xs) of
                Right _ -> assertFailure "Expected failure"
                Left _ -> return ()
