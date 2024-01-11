{-# LANGUAGE DeriveTraversable #-}
module TypedSyntax where

import Values (Lit)
import Var
import Types
import qualified HistPgm as Hist

data TermF a =
      TmLitR Lit
    | TmEpsR
    | TmVar Var
    | TmCatL Var Var Var a
    | TmCatR a a
    | TmParL Var Var Var a
    | TmParR a a
    | TmInl a
    | TmInr a
    | TmPlusCase Var Var a Var a
    | TmIte Var a a
    | TmNil
    | TmCons a a
    | TmStarCase Var a Var Var a
    | TmFix (CtxStruct a) a
    | TmRec (CtxStruct a)
    | TmWait Var a
    | TmCut Var a a
    | TmHistPgm Hist.Term
    deriving (Functor,Foldable,Traversable)

data Term = Term (Ctx Var Ty) (TermF Term) Ty
