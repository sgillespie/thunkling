module Language.Thunkling.Syntax
  ( Name,
    Phase (..),
    Ann (..),
    parsedAnn,
    parsedAnnEmpty,
    Program (..),
    TopLevelBind (..),
    Param (..),
    TyScheme (..),
    ExprTy (..),
    TyVar (..),
    Expr (..),
    Var (..),
    exprAnnL,
  ) where

import Lens.Micro

type Name = Text

data Phase
  = Parsed
  | Typechecked
  deriving stock
    ( Eq,
      Enum,
      Ord,
      Show
    )

data family Ann (phase :: Phase)

newtype instance Ann 'Parsed
  = ParsedAnn (Maybe ExprTy)
  deriving stock (Eq, Show)

parsedAnnEmpty :: Ann 'Parsed
parsedAnnEmpty = ParsedAnn Nothing

parsedAnn :: ExprTy -> Ann 'Parsed
parsedAnn = ParsedAnn . Just

newtype instance Ann 'Typechecked
  = TypedAnn ExprTy
  deriving stock (Eq, Show)

newtype Program phase = Program {unProgram :: [TopLevelBind phase]}

deriving instance Eq (Program 'Parsed)
deriving instance Show (Program 'Parsed)
deriving instance Eq (Program 'Typechecked)
deriving instance Show (Program 'Typechecked)

data TopLevelBind (phase :: Phase) = TopLevelBind
  { bindName :: Name,
    bindAnn :: Ann phase,
    bindParams :: [Param],
    bindExpr :: Expr phase
  }

deriving instance Eq (TopLevelBind 'Parsed)
deriving instance Show (TopLevelBind 'Parsed)
deriving instance Eq (TopLevelBind 'Typechecked)
deriving instance Show (TopLevelBind 'Typechecked)

newtype Param = Param {unParam :: Name}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data TyScheme = Forall [TyVar] ExprTy
  deriving stock (Eq, Show)

newtype TyVar = Tv {unTv :: Name}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data ExprTy
  = TyArrow ExprTy ExprTy
  | TyAbs TyScheme
  | TyVar Name
  | TyInt
  | TyBool
  | TyString
  | TyUnit
  deriving (Eq, Show)

data Expr (phase :: Phase) where
  Var :: Ann phase -> Var -> Expr phase
  App :: Ann phase -> Expr phase -> Expr phase -> Expr phase
  Abs :: Ann phase -> Param -> Expr phase -> Expr phase
  Add :: Ann phase -> Expr phase -> Expr phase -> Expr phase
  Sub :: Ann phase -> Expr phase -> Expr phase -> Expr phase
  LitInt :: Ann phase -> Int -> Expr phase
  LitBool :: Ann phase -> Bool -> Expr phase
  LitString :: Ann phase -> Text -> Expr phase
  LitUnit :: Ann phase -> Expr phase

deriving instance Eq (Expr 'Parsed)
deriving instance Show (Expr 'Parsed)
deriving instance Eq (Expr 'Typechecked)
deriving instance Show (Expr 'Typechecked)

newtype Var = V {unV :: Name}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

exprAnnL :: Lens' (Expr phase) (Ann phase)
exprAnnL = lens getAnn setAnn
  where
    getAnn expr = 
      case expr of
        Var ann _ -> ann
        App ann _ _ -> ann
        Abs ann _ _ -> ann
        Add ann _ _ -> ann
        Sub ann _ _ -> ann
        LitInt ann _ -> ann
        LitBool ann _ -> ann
        LitString ann _ -> ann
        LitUnit ann -> ann

    setAnn expr ann = 
      case expr of
        Var _ v -> Var ann v
        App _ e1 e2 -> App ann e1 e2
        Abs _ n body -> Abs ann n body
        Add _ e1 e2 -> Add ann e1 e2
        Sub _ e1 e2 -> Sub ann e1 e2
        LitInt _ i -> LitInt ann i
        LitBool _ b -> LitBool ann b
        LitString _ s -> LitString ann s
        LitUnit _ -> LitUnit ann

