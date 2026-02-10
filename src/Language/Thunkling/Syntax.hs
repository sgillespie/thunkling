module Language.Thunkling.Syntax
  ( Name,
    Phase (..),
    Ann (..),
    parsedAnn,
    parsedAnnEmpty,
    Program (..),
    TopLevelBind (..),
    TyScheme (..),
    Param (..),
    ExprTy (..),
    TyVar (..),
    Expr (..),
    Var (..),
    traverseExprAnn,
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

newtype instance Ann 'Parsed = ParsedAnn {unParsedAnn :: Maybe ExprTy}
  deriving stock (Eq, Show)

parsedAnnEmpty :: Ann 'Parsed
parsedAnnEmpty = ParsedAnn Nothing

parsedAnn :: ExprTy -> Ann 'Parsed
parsedAnn = ParsedAnn . Just

newtype instance Ann 'Typechecked = TypedAnn {unTypedAnn :: ExprTy}
  deriving stock (Eq, Show)

newtype Program phase = Program {unProgram :: [TopLevelBind phase]}

deriving instance Eq (Program 'Parsed)
deriving instance Show (Program 'Parsed)
deriving instance Eq (Program 'Typechecked)
deriving instance Show (Program 'Typechecked)

data TopLevelBind (phase :: Phase) = TopLevelBind
  { bindVar :: Var (Ann phase),
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
  deriving stock (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data Expr (phase :: Phase) where
  Var :: Var (Ann phase) -> Expr phase
  App :: Expr phase -> Expr phase -> Expr phase
  Abs :: Var (Ann phase) -> Expr phase -> Expr phase
  LitInt :: Int -> Expr phase
  LitBool :: Bool -> Expr phase
  LitString :: Text -> Expr phase
  LitUnit :: Expr phase

traverseExprAnn :: Traversal' (Expr 'Typechecked) (Ann 'Typechecked)
traverseExprAnn f (Var (V n ann)) = Var . V n <$> f ann
traverseExprAnn f (App e1 e2) = App <$> traverseExprAnn f e1 <*> traverseExprAnn f e2
traverseExprAnn f (Abs (V n ann) e) = (Abs . V n <$> f ann) <*> traverseExprAnn f e
traverseExprAnn _ expr@LitInt{} = pure expr
traverseExprAnn _ expr@LitBool{} = pure expr
traverseExprAnn _ expr@LitString{} = pure expr
traverseExprAnn _ expr@LitUnit = pure expr

deriving instance Eq (Expr 'Parsed)
deriving instance Show (Expr 'Parsed)
deriving instance Eq (Expr 'Typechecked)
deriving instance Show (Expr 'Typechecked)

data Var ty = V
  { vName :: Name,
    vType :: ty
  }
  deriving stock (Eq, Ord, Show)
