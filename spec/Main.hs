{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Text as A
import           Data.Either          (rights)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Language.Css.Parse   as P
import qualified Language.Css.Syntax  as C
import           Language.Lss
import           Test.Hspec

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

type TConst = (Text, Text)
type TFunc = ([Text], [TConst], [Text])

buildState :: [(Text, TFunc)] -> [TConst] -> LssState
buildState fs cs = LssState (M.fromList (consts cs)) (M.fromList (funcs fs))
  where consts = map (\(i,e) -> let Right expr = A.parseOnly P.exprp e
                                 in (C.Ident (T.unpack i), expr))
        funcs = map (\(i, (ps, lcs, b)) -> let params = rights (map (A.parseOnly P.identp) ps)
                                               cs' = M.fromList $ consts lcs
                                               rules = rights (map (A.parseOnly P.rulesetp) b)
                                           in (C.Ident (T.unpack i), LssFunc params cs' rules))

main :: IO ()
main = hspec $ do
  describe "parseDefs" $ do
    it "should parse a bare const" $ do
      parseDefs "x = #ccc" `shouldBe` (Right (buildState [] [("x", "#ccc")]))
    it "should parse two consts" $ do
      parseDefs "x = #ccc\n y = 10em" `shouldBe` (Right (buildState [] [ ("x", "#ccc")
                                                                       , ("y", "10em")]))
    it "should parse an argumentless, empty function" $ do
      parseDefs "x {}" `shouldBe` (Right (buildState [("x", ([],[],[]))] []))
    it "should parse an argumentless, empty function with parens" $ do
      parseDefs "x() {}" `shouldBe` (Right (buildState [("x", ([],[],[]))] []))
    it "should not parse a function with a space before parens" $ do
      parseDefs "x () {}" `shouldSatisfy` isLeft
    it "should parse an argumentless empty fuction with some css inside" $ do
      parseDefs "x { p { font-size: 1em; } }" `shouldBe`
        (Right (buildState [("x", ([],[],["p { font-size: 1em; }"]))] []))
    it "should parse local constants within a function" $ do
      parseDefs "x { y = 10em }" `shouldBe` (Right (buildState [("x", ([],[("y", "10em")],[]))]
                                                               []))
    it "should parse arguments in a function" $ do
      parseDefs "x(size, weight) {}" `shouldBe`
        (Right (buildState [("x", (["size", "weight"],[],[]))] []))
