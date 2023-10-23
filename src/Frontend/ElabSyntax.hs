{-# LANGUAGE FlexibleContexts #-}
module Frontend.ElabSyntax where

import Values ( Lit(..) )
import Var (Var(..))
import Control.Monad.State (MonadState (put), get)
import Control.Monad.Except (MonadError)

import qualified Frontend.SurfaceSyntax as Surf

data Term =
      TmLitR Lit
    | TmEpsR
    | TmVar Var
    | TmCatL Var Var Var Term
    | TmCatR Term Term
    | TmInl Term
    | TmInr Term
    | TmPlusCase Var Var Term Var Term
    | TmCut Var Term Term
    deriving (Eq, Ord, Show)

data ElabState = ES { nextVar :: Int }

class (MonadState ElabState m) => ElabM m where

freshElabVar :: (ElabM m) => m Var
freshElabVar = do
    es <- get
    put $ ES (nextVar es + 1)
    return $ Var.Var $ "__x" ++ show (nextVar es)

elabMaybeVar :: (ElabM m) => Maybe Var -> m Var
elabMaybeVar Nothing = freshElabVar
elabMaybeVar (Just x) = return x

elab :: (ElabM m) => Surf.Term -> m Term
elab (Surf.TmLitR l) = return (TmLitR l)
elab Surf.TmEpsR = return TmEpsR
elab (Surf.TmVar x) = return (TmVar x)
elab (Surf.TmCatL mx my e1 e2) = do
    e1' <- elab e1
    e2' <- elab e2
    z <- freshElabVar
    x <- elabMaybeVar mx
    y <- elabMaybeVar my
    return $ TmCut z e1' (TmCatL x y z e2')
elab (Surf.TmCatR e1 e2) = do
    e1' <- elab e1
    e2' <- elab e2
    return (TmCatR e1' e2')
elab (Surf.TmInl e) = TmInl <$> elab e
elab (Surf.TmInr e) = TmInr <$> elab e
elab (Surf.TmPlusCase e mx e1 my e2) = do
    e' <- elab e
    e1' <- elab e1
    e2' <- elab e2
    z <- freshElabVar
    x <- elabMaybeVar mx
    y <- elabMaybeVar my
    return $ TmCut z e' (TmPlusCase z x e1' y e2')

{-TODO: ensure that fundefs have unique vars-}