{-# LANGUAGE LambdaCase #-}

module Language.Thunkling.Typecheck
  ( TypeError (..),
    typecheck,
  ) where

import Language.Thunkling.Syntax

import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.RWS (MonadWriter (..), RWST, evalRWST)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.Thunkling.Pretty (showTy)
import Lens.Micro ((%~))
import Relude.Unsafe ((!!))
import Text.Show qualified as Show
import Data.Foldable (Foldable(..), foldrM)

data TypeError
  = UnboundVariable Name
  | InfiniteType TyVar ExprTy
  | IllegalRank ExprTy
  | UnificationFail ExprTy ExprTy
  | Impossible
  deriving stock (Eq)

instance Show TypeError where
  show (UnboundVariable n) =
    "Variable not in scope: " <> Text.unpack n
  show (InfiniteType (Tv v) ty) =
    "Cannot construct the infinite type: "
      <> Text.unpack v
      <> " = "
      <> Text.unpack (showTy ty)
  show (IllegalRank ty) =
    "Illegal polymorphic type: " <> Text.unpack (showTy ty)
  show (UnificationFail t1 t2) =
    "Cannot match `"
      <> Text.unpack (showTy t1)
      <> "` with expected type `"
      <> Text.unpack (showTy t2)
      <> "`"
  show Impossible =
    "Impossible error: This indicates a bug in the compiler's logic. Please submit a bug"

instance Exception TypeError

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

data TyConstraint = TyConstraint ExprTy ExprTy
  deriving stock (Eq, Ord, Show)

newtype InferState = InferState {stateCount :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Num)

runInfer :: TyEnv -> Infer a -> Either TypeError (a, [TyConstraint])
runInfer env (Infer m) =
  runExcept $
    evalRWST
      m
      env
      initInferState

emptyTyEnv :: TyEnv
emptyTyEnv =
  TyEnv $
    Map.fromList
      [ ("*", Forall [] (TyInt `TyArrow` (TyInt `TyArrow` TyInt))),
        ("/", Forall [] (TyInt `TyArrow` (TyInt `TyArrow` TyInt))),
        ("+", Forall [] (TyInt `TyArrow` (TyInt `TyArrow` TyInt))),
        ("-", Forall [] (TyInt `TyArrow` (TyInt `TyArrow` TyInt))),
        ("println", Forall [] (TyString `TyArrow` TyUnit))
      ]

extendTyEnv :: TyEnv -> (Name, TyScheme) -> TyEnv
extendTyEnv (TyEnv env) (v, scheme) = TyEnv $ Map.insert v scheme env

removeTyEnv :: TyEnv -> Name -> TyEnv
removeTyEnv (TyEnv env) v = TyEnv (Map.delete v env)

lookupTyEnv :: Name -> Infer ExprTy
lookupTyEnv v = do
  TyEnv env <- ask
  case Map.lookup v env of
    Nothing -> throwError (UnboundVariable v)
    Just scheme -> instantiate scheme

inEnv :: (Name, TyScheme) -> Infer a -> Infer a
inEnv (var, scheme) = local addScope
  where
    addScope env = extendTyEnv (removeTyEnv env var) (var, scheme)

unify :: ExprTy -> ExprTy -> Infer ()
unify t1 t2 = tell [TyConstraint t1 t2]

initInferState :: InferState
initInferState = 0

fresh :: Infer ExprTy
fresh = do
  count <- gets stateCount
  put $ InferState (count + 1)
  pure $ TyVar (Text.pack $ letters !! count)

letters :: [String]
letters = concatMap (`replicateM` ['a' .. 'z']) [1 ..]

instantiate :: TyScheme -> Infer ExprTy
instantiate (Forall vs ty) = do
  vs' <- mapM (const fresh) vs
  let sub = TySub $ Map.fromList (zip vs vs')

  pure $ applySub sub ty

newtype Solve a
  = Solve
      ( StateT
          TyUnifier
          (Except TypeError)
          a
      )
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError TypeError,
      MonadState TyUnifier
    )

data TyUnifier = TyUnifier TySub [TyConstraint]
  deriving stock (Eq, Ord, Show)

newtype TySub = TySub {unTySub :: Map TyVar ExprTy}
  deriving stock (Eq, Ord, Show)

runSolve :: Solve TySub -> [TyConstraint] -> Either TypeError TySub
runSolve (Solve m) constraints =
  runExcept $ evalStateT m (TyUnifier emptySub constraints)

emptyUnifier :: TyUnifier
emptyUnifier = TyUnifier emptySub []

emptySub :: TySub
emptySub = TySub Map.empty

compose :: TySub -> TySub -> TySub
sub1 `compose` sub2 =
  TySub $
    Map.map (applySub sub1) (unTySub sub2) `Map.union` unTySub sub1

applySub :: TySub -> ExprTy -> ExprTy
applySub sub (t1 `TyArrow` t2) = applySub sub t1 `TyArrow` applySub sub t2
applySub sub ty@(TyVar v) = Map.findWithDefault ty (Tv v) (unTySub sub)
applySub sub (TyAbs scheme) = TyAbs (applySubScheme sub scheme)
applySub _ ty@TyInt = ty
applySub _ ty@TyBool = ty
applySub _ ty@TyString = ty
applySub _ ty@TyUnit = ty

applySubScheme :: TySub -> TyScheme -> TyScheme
applySubScheme (TySub sub) (Forall vs ty) = Forall vs (applySub sub' ty)
  where
    sub' = TySub (foldr Map.delete sub vs)

applySubConstraint :: TySub -> TyConstraint -> TyConstraint
applySubConstraint sub (TyConstraint t1 t2) =
  TyConstraint (applySub sub t1) (applySub sub t2)

bind :: TyVar -> ExprTy -> Solve TyUnifier
bind v ty
  | ty == TyVar (unTv v) = pure emptyUnifier
  | failsOccursCheck v ty = throwError (InfiniteType v ty)
  | otherwise = pure $ TyUnifier (TySub $ Map.singleton v ty) []

failsOccursCheck :: TyVar -> ExprTy -> Bool
failsOccursCheck v ty = v `Set.member` freeTyVars ty

freeTyVars :: ExprTy -> Set TyVar
freeTyVars (t1 `TyArrow` t2) = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (TyVar v) = Set.singleton (Tv v)
freeTyVars TyInt = Set.empty
freeTyVars TyBool = Set.empty
freeTyVars TyString = Set.empty
freeTyVars TyUnit = Set.empty
freeTyVars (TyAbs (Forall vs innerTy)) =
  freeTyVars innerTy `Set.difference` Set.fromList vs

typecheck :: Program 'Parsed -> Either TypeError (Program 'Typechecked)
typecheck (Program binds) = Program <$> evalStateT (mapM typecheck' binds) emptyTyEnv

typecheck' 
  :: TopLevelBind 'Parsed 
  -> StateT TyEnv (Either TypeError) (TopLevelBind 'Typechecked)
typecheck' bind' = do
  env <- get

  res@TopLevelBind{..} <- lift $ typecheckBind env bind'
  let V{vName, vType = TypedAnn ty} = bindVar

  put $ env `extendTyEnv` (vName, mkScheme ty)

  pure res
  where
    mkScheme (TyAbs scheme) = scheme
    mkScheme ty = Forall [] ty

typecheckBind :: TyEnv -> TopLevelBind 'Parsed -> Either TypeError (TopLevelBind 'Typechecked)
typecheckBind env binding = do
  runExcept $ do
    (inferred, constraints) <- hoistEither $ runInfer env (inferTopBind binding)
    solved <- hoistEither $ runSolve solveConstraints constraints

    let
      TopLevelBind{bindVar, bindExpr} = inferred
      V n (TypedAnn ty) = bindVar

    -- Apply substitutions to calculate the final type
    ty' <- hoistEither . closeOver . applySub solved . TyAbs $ Forall [] ty
    -- Apply substitutions in expr nodes
    let bindExpr' =
          bindExpr & traverseExprAnn %~ (TypedAnn . applySub solved . unTypedAnn)

    pure $
      inferred
        { bindVar = V n (TypedAnn ty'),
          bindExpr = bindExpr'
        }

inferTopBind :: TopLevelBind 'Parsed -> Infer (TopLevelBind 'Typechecked)
inferTopBind TopLevelBind{..} = do
  let V varName (ParsedAnn varAnnTy) = bindVar

  -- Generate a fresh type variable for each param
  params' <- for bindParams $ \param -> do
    tv <- fresh
    pure (coerce param, tv)

  -- Add the params with their types to the type environment, and run inference
  bindExpr' <- inferBindExpr' params' (inferExpr bindExpr)
  bodyTy <- typeOf bindExpr'

  let varTy = inferTopTy (map snd params') bodyTy

  -- Emit a constraint on the declared type, if explicit
  mapM_ (unify varTy) varAnnTy

  pure $
    TopLevelBind
      { bindVar = V varName (TypedAnn varTy),
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
        Just paramTys' -> foldr TyArrow bodyTy paramTys'

inferExpr :: Expr 'Parsed -> Infer (Expr 'Typechecked)
inferExpr expr = do
  case expr of
    Var (V v _) -> do
      t <- lookupTyEnv v
      pure $ Var (V v (TypedAnn t))
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
        TyAbs{} -> throwError $ IllegalRank t1
        _ `TyArrow` t2' -> pure t2'
        _ -> pure t1 -- This is probably an error, but we'll defer to the constraint solver
    Abs (V _ (TypedAnn varTy)) body -> TyArrow varTy <$> typeOf body
    LitInt _ -> pure TyInt
    LitBool _ -> pure TyBool
    LitString _ -> pure TyString
    LitUnit -> pure TyUnit

solveConstraints :: Solve TySub
solveConstraints = do
  TyUnifier sub constraints <- get
  case constraints of
    [] -> pure sub
    (TyConstraint t1 t2 : constraints0) -> do
      TyUnifier sub1 constraints1 <- t1 `unifies` t2
      put $
        TyUnifier
          (sub1 `compose` sub)
          (constraints1 ++ map (applySubConstraint sub1) constraints0)

      solveConstraints

unifies :: ExprTy -> ExprTy -> Solve TyUnifier
unifies t1 t2 | t1 == t2 = pure emptyUnifier
unifies (TyVar v) t2 = Tv v `bind` t2
unifies t1 (TyVar v) = Tv v `bind` t1
unifies (t1 `TyArrow` t1') (t2 `TyArrow` t2') = unifies2 (t1, t1') (t2, t2')
unifies t1 t2 = throwError (UnificationFail t1 t2)

unifies2 :: (ExprTy, ExprTy) -> (ExprTy, ExprTy) -> Solve TyUnifier
unifies2 (t1, t2) (t1', t2') = do
  TyUnifier sub1 constraints1 <- unifies t1 t1'
  TyUnifier sub2 constraints2 <- unifies t2 t2'

  pure $ TyUnifier (sub2 `compose` sub1) (constraints1 ++ constraints2)

closeOver :: ExprTy -> Either TypeError ExprTy
closeOver (TyAbs scheme) =
  flip second (closeOverScheme scheme) $ \case
    Forall [] ty -> ty
    scheme' -> TyAbs scheme'
closeOver ty = Right ty

closeOverScheme :: TyScheme -> Either TypeError TyScheme
closeOverScheme (Forall _ ty) =
  Forall (map snd (Map.toList freeVars)) <$> normTy ty
  where
    freeVars =
      Map.fromList $
        zip (toList $ freeTyVars ty) (map (Tv . Text.pack) letters)

    normTy :: ExprTy -> Either TypeError ExprTy
    normTy (t1 `TyArrow` t2) = TyArrow <$> normTy t1 <*> normTy t2
    normTy t@(TyAbs{}) = Left (IllegalRank t)
    normTy TyInt = Right TyInt
    normTy TyBool = Right TyBool
    normTy TyString = Right TyString
    normTy TyUnit = Right TyUnit
    normTy (TyVar v) =
      case Map.lookup (Tv v) freeVars of
        Just (Tv v') -> Right (TyVar v')
        Nothing -> Left Impossible
