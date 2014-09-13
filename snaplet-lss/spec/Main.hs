{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

----------------------------------------------------------
-- Section 0: Imports.                                  --
----------------------------------------------------------
import           Control.Lens
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Snap               (Handler, Method (..), Snaplet, addRoutes,
                                     makeSnaplet, nestSnaplet, route,
                                     subSnaplet)
import           Snap.Snaplet.Heist

import           Test.Hspec
import           Test.Hspec.Snap

import           Snap.Snaplet.Lss

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------

data App = App { _heist :: Snaplet (Heist App), _lss :: Snaplet Lss }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

routes = [("test", render "test")
         ,("test2", render "test2")]

app = makeSnaplet "app" "An snaplet example application." Nothing $ do
         addRoutes routes
         h <- nestSnaplet "" heist $ heistInit "templates"
         l <- nestSnaplet "" lss $ initLss lss h
         return $ App h l


----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------

main :: IO ()
main = hspec $ snap (route routes) app $ do
  describe "basic tests" $ do
    it "should have style tag when applying function" $ do
      p <- get "/test"
      p `shouldHaveSelector` "style"
      p `shouldHaveText` "background-color"
      p `shouldHaveText` "red"
    it "should substitute params into css rules" $ do
      p <- get "/test2"
      -- NOTE(dbp 2014-09-04): The CSS library renders with odd spacing and treatment of numbers.
      p `shouldHaveText` "font-size : 10.0em"
