module Language.Thunkling.Syntax
  ( Name (..),
    Phase (..),
    Ann (..),
    Program (..),
    TopLevelBind (..),
    ExprTy (..),
    Expr (..),
  ) where

newtype Name = Name {unName :: ByteString}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data Phase
  = Parsed
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

newtype Program phase = Program {unProgram :: [TopLevelBind phase]}

deriving instance Eq (Program 'Parsed)
deriving instance Show (Program 'Parsed)

data TopLevelBind (phase :: Phase)
  = TopLevelBind
  { bindName :: Name,
    bindAnn :: Ann phase,
    bindExpr :: Expr phase
  }

deriving instance Eq (TopLevelBind 'Parsed)
deriving instance Show (TopLevelBind 'Parsed)

data ExprTy
  = TyInt
  | TyBool
  | TyUnit
  deriving (Eq, Show)

data Expr (phase :: Phase) where
  LitInt :: Ann phase -> Int -> Expr phase
  LitBool :: Ann phase -> Bool -> Expr phase

deriving instance Eq (Expr 'Parsed)
deriving instance Show (Expr 'Parsed)
