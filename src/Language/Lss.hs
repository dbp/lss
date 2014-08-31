{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lss where

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Language.Css.Syntax as C
import           Prelude             hiding ((++))
import qualified Text.XmlHtml        as X

(++) :: Monoid d => d -> d -> d
(++) = mappend

type ConstMap = Map C.Ident C.Expr

deriving instance Ord C.Ident
unIdent :: C.Ident -> Text
unIdent (C.Ident s) = T.pack s

data LssState = LssState { sConstants :: ConstMap, sFunctions ::  Map C.Ident LssFunc}

data LssFunc = LssFunc { fParams :: [C.Ident], fLocalConstants :: ConstMap, fBody :: [C.RuleSet] }

instance Monoid LssState where
  mempty = LssState M.empty M.empty
  mappend (LssState c1 f1) (LssState c2 f2) = LssState (M.union c2 c1) (M.union f2 f1)


parse :: Text -> Either Text LssState
parse = undefined

apply :: LssState -> C.Ident -> [C.Expr] -> Either Text [C.RuleSet]
apply state ident args = case M.lookup ident (sFunctions state) of
                           Nothing -> Left $ "Unknown function: " ++ unIdent ident ++ "."
                           Just (LssFunc params locals body) | length params == length args ->
                             let localSubst = map (uncurry subst) (M.assocs locals)
                             in Right $ map (\b -> foldr ($) b (zipWith subst params args ++ localSubst)) body
                           Just (LssFunc params _ _) ->
                             Left $ "Invalid number of arguments to " ++ unIdent ident
                                 ++ ": expected " ++ T.pack (show $ length params)
                                 ++ ", but got " ++ T.pack (show $ length args)
                                 ++ " instead."
  where subst param arg (C.RuleSet sels decls) = C.RuleSet sels (map (substDecl param arg) decls)
        substDecl param arg (C.Decl prio prop expr) = C.Decl prio prop (substExpr param arg expr)
        substExpr param arg expr =
          case expr of
            C.EVal val -> case val of
                            C.VIdent ide | ide == param -> arg
                            _ -> expr
            C.SlashSep exp1 exp2 -> C.SlashSep (substExpr param arg exp1) (substExpr param arg exp2)
            C.CommaSep exp1 exp2 -> C.CommaSep (substExpr param arg exp1) (substExpr param arg exp2)
            C.SpaceSep exp1 exp2 -> C.SpaceSep (substExpr param arg exp1) (substExpr param arg exp2)

attach :: [C.RuleSet] -> [X.Node] -> [X.Node]
attach = undefined
