{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Lss (Lss(..), initLss, lssSplices) where

import           Control.Lens        (set, view)
import           Control.Monad       (foldM, when)
import           Control.Monad.Trans (lift, liftIO)
import           Data.List           (isSuffixOf)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Unique
import           Heist               (Splices, getParamNode,
                                      scInterpretedSplices, ( ## ))
import           Heist.Interpreted   (Splice, runChildren)
import           Prelude             hiding ((++))
import           Snap
import           Snap.Snaplet.Heist  (Heist, addConfig)
import           System.Directory
import           System.FilePath
import qualified Text.XmlHtml        as X

import           Language.Lss

(++) :: Monoid d => d -> d -> d
(++) = mappend

data Lss = Lss { lssCount :: Int }

initLss :: SnapletLens b Lss -> Snaplet (Heist b) -> SnapletInit b Lss
initLss lens heist = makeSnaplet "lss" "" Nothing $ do
  dir <- getSnapletFilePath
  dirExists <- liftIO $ doesDirectoryExist dir
  when (not dirExists) $ liftIO $ createDirectory dir
  files <- map (dir </>) <$> liftIO (getDirectoryContents dir)
  st <- foldM (\s f -> do isF <- liftIO $ doesFileExist f
                          if not isF || not (".lss" `isSuffixOf` f)
                             then return s
                             else do
                              c <- liftIO $ T.readFile f
                              case parseDefs c of
                                Left err -> error $ "Lss: Error parsing file " ++ f ++ ": " ++ err
                                Right stat -> return $ mappend s stat)
              mempty
              files
  addConfig heist (set scInterpretedSplices (lssSplices lens st) mempty)
  return (Lss 0)


lssSplices :: SnapletLens b Lss -> LssState -> Splices (Splice (Handler b b))
lssSplices lens st = "lss" ## lssSplice
  where lssSplice =
          do n <- getParamNode
             case X.getAttribute "apply" n of
               Nothing -> error "Lss: Can't apply <lss> tag without `apply` attribute."
               Just a ->
                 case parseApp a of
                   Left err -> error $ "Lss: error parsing `apply` attribute: " ++ err
                   Right (LssApp ident args) ->
                     do case apply st ident args  of
                          Left err -> error $ "Lss: error applying " ++ (T.unpack $ unIdent ident)
                                           ++ ": " ++ T.unpack err
                          Right rules -> do
                            childs' <- runChildren
                            let childs = case X.getAttribute "class" n of
                                           Nothing -> childs'
                                           Just cls -> map (addClass cls) childs'
                            count <- lift $ with lens $
                                       do s <- getSnapletState
                                          let (Lss n) = view snapletValue s
                                          putSnapletState (set snapletValue (Lss (n+1)) s)
                                          return n
                            let sym = ("lss" ++) . show $ count
                            return $ attach sym rules childs
          where addClass newCls n@(X.Element _ attrs _) =
                  let classAttr = case lookup "class" attrs  of
                                    Nothing -> ("class", newCls)
                                    Just cls -> ("class", newCls ++ " " ++ cls)
                  in n { X.elementAttrs = classAttr : filter ((/= "class").fst) attrs}
                addClass _ n = n
