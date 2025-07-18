module Language.Thunkling.Typecheck
  ( TypeError(..),
    typecheck,
  ) where

import Language.Thunkling.Syntax

import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.RWS (MonadWriter (..), RWST, evalRWST)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Lens.Micro
import Text.Show (Show (..))
import Relude.Unsafe ((!!))
import Data.Traversable (for)
import Data.Foldable (foldr1)

newtype Infer a
  = Infer
      ( RWST
          TyEnv
          [TyConstraint]
          InferState
          (Except TypeError)
          a
      )
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError TypeError,
      MonadState InferState,
      MonadReader TyEnv,
      MonadWriter [TyConstraint]
    )

newtype TyEnv = TyEnv {unTyEnv :: Map Var TyScheme}
  deriving stock (Eq, Show)

emptyTyEnv :: TyEnv
emptyTyEnv = TyEnv Map.empty

extendTyEnv :: TyEnv -> (Var, TyScheme) -> TyEnv
extendTyEnv (TyEnv env) (v, scheme) = TyEnv $ Map.insert v scheme env

removeTyEnv :: TyEnv -> Var -> TyEnv
removeTyEnv (TyEnv env) v = TyEnv (Map.delete v env)

lookupTyEnv :: TyEnv -> Var -> Maybe TyScheme
lookupTyEnv env = flip Map.lookup (unTyEnv env)

inEnv :: (Var, TyScheme) -> Infer a -> Infer a
inEnv (var, scheme) = local addScope
  where addScope env = extendTyEnv (removeTyEnv env var) (var, scheme)

data TyConstraint = TyConstraint ExprTy ExprTy
  deriving stock (Eq, Show)

unify :: ExprTy -> ExprTy -> Infer ()
unify t1 t2 = tell [TyConstraint t1 t2]

newtype InferState = InferState {stateCount :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Num)

initInferState :: InferState
initInferState = 0

fresh :: Infer ExprTy
fresh = do
  count <- gets stateCount
  put $ InferState (count + 1)
  pure $ TyVar (Text.pack $ letters !! count)
  where
    letters = concatMap (`replicateM` ['a' .. 'z']) [1 ..]

data TypeError
  = UnboundVariable Var
  | Impossible
  deriving stock (Eq)

instance Show TypeError where
  show (UnboundVariable (V n)) =
    "Variable not in scope: " <> Text.unpack n
  show Impossible =
    "Impossible error: This indicates a bug in the compiler's logic. Please submit a bug"

instance Exception TypeError

runInfer :: Infer a -> Either TypeError (a, [TyConstraint])
runInfer (Infer m) =
  runExcept $
    evalRWST
      m
      emptyTyEnv
      initInferState

typecheck :: Program 'Parsed -> Either TypeError (Program 'Typechecked)
typecheck (Program binds) = do
  binds' <- mapM (fmap fst . runInfer . inferTopBind) binds
  Right (Program binds')

inferTopBind :: TopLevelBind 'Parsed -> Infer (TopLevelBind 'Typechecked)
inferTopBind TopLevelBind{..} = do
  -- Generate a fresh type variable for each param
  params' <- for bindParams $ \param -> do
    tv <- fresh
    pure (coerce param, tv)

  -- Add the params with their types to the type environment, and run inference
  (bodyTy, bindExpr') <- inferBindExpr' params' (inferExpr' bindExpr)

  pure $
    TopLevelBind
      { bindName = bindName,
        bindAnn = TypedAnn $ inferTopTy (map snd params') bodyTy,
        bindParams = bindParams,
        bindExpr = bindExpr'
      }
  where
    inferBindExpr' params = local (`extendAllTyEnv` params)

    extendAllTyEnv = 
      foldr $ \(var, ty) env' -> 
        extendTyEnv env' (var, Forall [] ty)

    inferTopTy paramTys bodyTy = 
      case nonEmpty paramTys of
        Nothing -> bodyTy
        Just paramTys' -> foldr1 (flip TyArrow) paramTys'

inferExpr :: Expr 'Parsed -> Infer (Expr 'Typechecked)
inferExpr expr = do
  env <- ask

  case expr of
    Var _ v -> do
      maybe
        (throwError $ UnboundVariable v)
        (pure . flip Var v . TypedAnn . TyAbs)
        (env `lookupTyEnv` v)
    App _ e1 e2 -> do
      (t1, e1') <- inferExpr' e1
      (t2, e2') <- inferExpr' e2
      tv <- fresh

      unify t1 (t2 `TyArrow` tv)

      pure $ App (TypedAnn tv) e1' e2'
    Abs _ n body -> do
      tv <- fresh
      (tyBody, body') <- (coerce n, Forall [] tv) `inEnv` inferExpr' body

      pure $ Abs (TypedAnn $ tv `TyArrow` tyBody) n body'
    Add _ e1 e2 -> do
      inferBinOp e1 e2 (Add . TypedAnn)
    Sub _ e1 e2 -> do
      inferBinOp e1 e2 (Sub . TypedAnn)
    LitInt _ i ->
      pure $ LitInt (TypedAnn TyInt) i
    LitBool _ b ->
      pure $ LitBool (TypedAnn TyBool) b
    LitString _ s ->
      pure $ LitString (TypedAnn TyString) s
    LitUnit _ ->
      pure $ LitUnit (TypedAnn TyUnit)

inferExpr' :: Expr 'Parsed -> Infer (ExprTy, Expr 'Typechecked)
inferExpr' expr = do
  expr' <- inferExpr expr
  let (TypedAnn ty) = expr' ^. exprAnnL
  pure (ty, expr')

inferBinOp 
  :: Expr 'Parsed 
  -> Expr 'Parsed
  -> ( ExprTy
       -> Expr 'Typechecked 
       -> Expr 'Typechecked
       -> a
     )
  -> Infer a
inferBinOp e1 e2 f = do
  (t1, e1') <- inferExpr' e1
  (t2, e2') <- inferExpr' e2

  tv <- fresh

  unify 
    (t1 `TyArrow` (t2 `TyArrow` tv))
    (TyInt `TyArrow` (TyInt `TyArrow` TyInt))

  pure (f tv e1' e2')
