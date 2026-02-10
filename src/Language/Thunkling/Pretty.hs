{-# LANGUAGE LambdaCase #-}
module Language.Thunkling.Pretty 
  ( showProgramParsed,
    showProgramTypechecked,
    showTy,
    showPpr,
    pprProgramParsed,
    pprProgramTypechecked,
    pprExprParsed,
    pprExprTypechecked,
    pprTy,
  ) where

import Language.Thunkling.Syntax (Program (..), TopLevelBind (..), Var (..), Ann (..), Expr (..), Phase (..), ExprTy (..), TyScheme (..), TyVar (..), Param (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text (renderStrict)

-- Pretty print a program as @Text@.
showProgramParsed :: Program 'Parsed -> Text
showProgramParsed = showProgram

-- | Pretty print a program as @Text@ after typechecking with explicit type annotations.
showProgramTypechecked :: Program 'Typechecked -> Text
showProgramTypechecked = showProgram

showProgram :: PprTypeAnn p => Program p -> Text
showProgram = showPpr . pprProgram

showTy :: ExprTy -> Text
showTy = showPpr . pprTy

-- | Render a @Doc@ as @Text@
showPpr :: Doc ann -> Text
showPpr = renderStrict . mkLayout
  where
    mkLayout = Pretty.layoutPretty Pretty.defaultLayoutOptions

-- | Pretty print a program as a @Doc@
pprProgramParsed :: Program 'Parsed -> Doc ann
pprProgramParsed = pprProgram

-- | Pretty print a program as a @Doc@ after typechecking with explicit type annotations.
pprProgramTypechecked :: Program 'Typechecked -> Doc ann
pprProgramTypechecked = pprProgram

-- | Pretty print an @Expr@ as a @Doc@
pprExprParsed :: Expr 'Parsed -> Doc ann
pprExprParsed = pprExpr

-- | Pretty print an @ExprTy@ as a @Doc@ 
pprExprTypechecked :: Expr 'Typechecked -> Doc ann
pprExprTypechecked = pprExpr

-- | Pretty print a type annotation. If there is no explicit type, nothing should be
-- emitted.
class PprTypeAnn phase where
  -- | Pretty print a top level function signature. If anything is omitted, it should
  -- contain a trailing newline
  pprTopLevelAnn :: Var (Ann phase) -> Doc ann

  -- | Pretty print an inline type.
  pprInlineAnn :: Var (Ann phase) -> Doc ann

-- | A parsed program might have functions with type annotations. If so, it emits the
-- signature, otherwise nothing is emitted.
instance PprTypeAnn 'Parsed where
  pprTopLevelAnn (V _ (ParsedAnn Nothing)) = Pretty.emptyDoc
  pprTopLevelAnn (V v (ParsedAnn (Just ty))) = pprSignature v ty <> Pretty.line

  pprInlineAnn _ = Pretty.emptyDoc

-- | A typechecked program will have program will have explicit type annotations on vars,
-- parameters and functions.
instance PprTypeAnn 'Typechecked where
  pprTopLevelAnn (V v (TypedAnn ty)) = pprSignature v ty <> Pretty.line

  pprInlineAnn (V _ (TypedAnn ty)) = 
      -- We want to avoid littering the output with foralls and arrows, so so we'll only
      -- process the simplest cases: literals and type variables
      maybe Pretty.emptyDoc ((":" <>) . pprTy) (simpleTy ty)
    where
      simpleTy = \case
        (TyArrow _ _) -> Nothing
        (TyAbs (Forall [] inner@(TyVar _))) -> Just inner
        (TyAbs _) -> Nothing
        (TyVar _) -> Just ty
        TyInt -> Just ty
        TyBool -> Just ty
        TyUnit -> Just ty 
        TyString -> Just ty

pprSignature :: Pretty.Pretty a => a -> ExprTy -> Doc ann
pprSignature name ty = 
  Pretty.pretty name <+> 
  Pretty.colon <+> 
  pprTy ty

pprProgram :: PprTypeAnn p => Program p -> Doc ann
pprProgram (Program binds) = 
  vsep2 (map pprBind binds) <>  -- Print bindings between 2 blank lines
  Pretty.line                   -- Add a newline to the end
  where 
    vsep2 = Pretty.concatWith (\ x y -> x <> blankLine <> y)
    blankLine = Pretty.line <> Pretty.line

pprBind :: PprTypeAnn p => TopLevelBind p -> Doc ann
pprBind TopLevelBind{..} = 
  pprTopLevelAnn bindVar <> 
  pprLhs <+>
  "=" <+> pprExpr bindExpr
  where
    pprLhs = 
      -- Construct a flat list of @Doc@s, then space separate them. This is to make sure
      -- we only print a single space at the end whether or not there are parameters
      Pretty.hsep $
        Pretty.pretty (vName bindVar) : map (Pretty.pretty . unParam) bindParams

pprVar :: PprTypeAnn phase => Var (Ann phase) -> Doc ann
pprVar v@V{..}= Pretty.pretty vName <> pprInlineAnn v

pprTy :: ExprTy -> Doc ann
pprTy (TyArrow t1 t2) = pprTy t1 <+> "->" <+> pprTy t2
pprTy (TyAbs scheme) = pprTyScheme scheme
pprTy (TyVar v) = Pretty.pretty v
pprTy TyInt = "Int"
pprTy TyBool = "Bool"
pprTy TyString = "String"
pprTy TyUnit = "()"

pprTyScheme :: TyScheme -> Doc ann
pprTyScheme (Forall vars tyBody) =
  "forall" <+>
  Pretty.hsep (map (Pretty.pretty . unTv) vars) <>
  Pretty.dot <+>
  pprTy tyBody

pprExpr :: PprTypeAnn phase => Expr phase -> Doc ann
pprExpr (Var v) = pprVar v
pprExpr (App e1 e2) = pprApp e1 e2
pprExpr (Abs v body) = "\\" <+> pprVar v <> "." <+> pprExpr body
pprExpr (LitInt i) = Pretty.pretty i
pprExpr (LitBool b) = Pretty.pretty b
pprExpr (LitString s) = Pretty.dquotes (Pretty.pretty s)
pprExpr LitUnit = "()"

pprApp :: PprTypeAnn phase => Expr phase -> Expr phase -> Doc ann
pprApp (App (Var V{vName}) e1) e2 = pprBinOp e1 vName e2
pprApp e1 e2 = pprOperand e1 <+> pprOperand e2

pprBinOp :: PprTypeAnn phase => Expr phase -> Text -> Expr phase -> Doc ann
pprBinOp e1 op e2 
  | op `elem` ["*", "/", "+", "-"] = pprExpr e1 <+> Pretty.pretty op <+> pprExpr e2
  | otherwise = Pretty.pretty op <+> pprOperand e1 <+> pprOperand e2

pprOperand :: PprTypeAnn phase => Expr phase -> Doc ann
pprOperand expr@Var{} = pprExpr expr
pprOperand expr@App{} = Pretty.parens (pprExpr expr)
pprOperand expr@Abs{} = Pretty.parens (pprExpr expr)
pprOperand expr@LitInt{} = pprExpr expr
pprOperand expr@LitBool{} = pprExpr expr
pprOperand expr@LitString{} = pprExpr expr
pprOperand expr@LitUnit = pprExpr expr
