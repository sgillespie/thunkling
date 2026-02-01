module Language.Thunkling.Typecheck
  ( TypeError(..),
    typecheck,
  ) where

import Language.Thunkling.Syntax

import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.RWS (MonadWriter (..), RWST, evalRWST)
import Data.Map qualified as Map
import Data.Text qualified as Text
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

newtype TyEnv = TyEnv {unTyEnv :: Map Name TyScheme}
  deriving stock (Eq, Show)

emptyTyEnv :: TyEnv
emptyTyEnv = 
  TyEnv $
    Map.fromList
      [ ("+", Forall [] (TyInt `TyArrow` (TyInt `TyArrow` TyInt))),
        ("println", Forall [] (TyString `TyArrow` TyUnit))
      ]

extendTyEnv :: TyEnv -> (Name, TyScheme) -> TyEnv
extendTyEnv (TyEnv env) (v, scheme) = TyEnv $ Map.insert v scheme env

removeTyEnv :: TyEnv -> Name -> TyEnv
removeTyEnv (TyEnv env) v = TyEnv (Map.delete v env)

lookupTyEnv :: TyEnv -> Name -> Maybe TyScheme
lookupTyEnv env = flip Map.lookup (unTyEnv env)

inEnv :: (Name, TyScheme) -> Infer a -> Infer a
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
  = UnboundVariable Name
  | Impossible
  deriving stock (Eq)

instance Show TypeError where
  show (UnboundVariable n) =
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
  let V varName _ = bindVar

  -- Generate a fresh type variable for each param
  params' <- for bindParams $ \param -> do
    tv <- fresh
    pure (coerce param, tv)

  -- Add the params with their types to the type environment, and run inference
  bindExpr' <- inferBindExpr' params' (inferExpr bindExpr)
  bodyTy <- typeOf bindExpr'

  let varTy = TypedAnn $ inferTopTy (map snd params') bodyTy

  pure $
    TopLevelBind
      { bindVar = V varName varTy,
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
    Var (V v _) -> do
      maybe
        (throwError $ UnboundVariable v)
        ( \t -> pure $ Var $ V v (TypedAnn $ TyAbs t))
        (env `lookupTyEnv` v)
    App e1 e2 -> do
      e1' <- inferExpr e1
      e2' <- inferExpr e2
      t1 <- typeOf e1'
      t2 <- typeOf e2'
      tv <- fresh

      unify t1 (t2 `TyArrow` tv)

      pure $ App e1' e2'
    Abs (V n _) body -> do
      tv <- fresh
      body' <- (coerce n, Forall [] tv) `inEnv` inferExpr body

      tyBody <- typeOf body'

      let v' = V n (TypedAnn $ tv `TyArrow` tyBody)

      pure $ Abs v' body'
    LitInt i -> pure (LitInt i)
    LitBool b -> pure (LitBool b)
    LitString s -> pure (LitString s)
    LitUnit -> pure LitUnit

typeOf :: Expr 'Typechecked -> Infer ExprTy
typeOf expr = do
  case expr of
    Var (V _ (TypedAnn t)) -> pure t
    App e1 _ -> do
      t1 <- typeOf e1
      case t1 of
        TyAbs (Forall [] (_ `TyArrow` t2')) -> pure t2'
        _ `TyArrow` t2' -> pure t2'
        _ -> 
          traceShow 
            t1
            (throwError Impossible)
    Abs (V _ (TypedAnn varTy)) body -> TyArrow varTy <$> typeOf body
    LitInt _ -> pure TyInt
    LitBool _ -> pure TyBool
    LitString _ -> pure TyString
    LitUnit -> pure TyUnit

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
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  t1 <- typeOf e1'
  t2 <- typeOf e2'

  tv <- fresh

  unify 
    (t1 `TyArrow` (t2 `TyArrow` tv))
    (TyInt `TyArrow` (TyInt `TyArrow` TyInt))

  pure (f tv e1' e2')
