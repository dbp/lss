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

buildApp :: Text -> [Text] -> LssApp
buildApp n as = let Right name = A.parseOnly P.identp n
                    args = rights (map (A.parseOnly P.exprp) as)
                 in LssApp name args

unRight :: Either a b -> b
unRight (Right v) = v
unRight _ = error "Expected right"

main :: IO ()
main = hspec $ do
  describe "parseDefs" $ do
    it "should parse a bare const" $ do
      parseDefs "x = #ccc" `shouldBe` (Right (buildState [] [("x", "#ccc")]))
    it "should parse two consts" $ do
      parseDefs "x = #ccc\n y = 10em" `shouldBe` (Right (buildState [] [ ("x", "#ccc")
                                                                       , ("y", "10em")]))
    it "should consts constants with newlines" $ do
      parseDefs "yellow = #fff200\n"
        `shouldBe` (Right (buildState [] [("yellow", "#fff200")]))
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
    it "should parse a one argument function" $ do
      parseDefs "x(size) {}" `shouldBe`
        (Right (buildState [("x", (["size"],[],[]))] []))
    it "should parse arguments in a function" $ do
      parseDefs "x(size, weight) {}" `shouldBe`
        (Right (buildState [("x", (["size", "weight"],[],[]))] []))
  describe "parseApp" $ do
    it "should parse bare function name" $
      parseApp "foo" `shouldBe` (Right (buildApp "foo" []))
    it "should parse a function with parens" $
      parseApp "foo()" `shouldBe` (Right (buildApp "foo" []))
    it "should parse a function with one argument" $
      parseApp "foo(#ccc)" `shouldBe` (Right (buildApp "foo" ["#ccc"]))
  describe "apply" $ do
    it "should produce body of argumentless function" $
      apply (unRight $ parseDefs "x { p { font-size: 1em; } }")
            (C.Ident "x")
            []
        `shouldBe` (Right [unRight $ A.parseOnly P.rulesetp "p { font-size: 1em; }"])
    it "should do replacement within function" $
      apply (unRight $ parseDefs "x(size) { p { font-size: size; } }")
            (C.Ident "x")
            [unRight $ A.parseOnly P.exprp "10em"]
        `shouldBe` (Right [unRight $ A.parseOnly P.rulesetp "p { font-size: 10em; }"])
    it "should replace global constants in blocks" $
      apply (unRight $ parseDefs "size = 10em\nx { p { font-size: size; } }")
            (C.Ident "x")
            []
        `shouldBe` (Right [unRight $ A.parseOnly P.rulesetp "p { font-size: 10em; }"])
