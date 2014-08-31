{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Lss where

import           Control.Monad      (when)
import           Data.List          (isSuffixOf)
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text.IO       as T
import           Data.Unique
import           Heist              (Splices, getParamNode,
                                     hcInterpretedSplices, ( ## ))
import           Heist.Interpreted  (Splice)
import           Snap
import           Snap.Snaplet.Heist (Heist, addConfig)
import           System.Directory
import qualified Text.XmlHtml       as X

import           Language.Lss

data Lss = Lss

initLss :: Snaplet (Heist b) -> SnapletInit b Lss
initLss heist = makeSnaplet "lss" "" Nothing $ do
  dir <- getSnapletFilePath
  dirExists <- liftIO $ doesDirectoryExist dir
  when (not dirExists) $ createDirectory dir
  files <- liftIO $ getDirectoryContents dir
  st <- foldM (\s f -> do isF <- liftIO $ doesFileExist f
                          if not isF || not (isSuffixOf ".lss" f)
                             then sundefined
                             else do
                              c <- liftIO $ T.readFile f
                              case parseDefs c of
                                Left err -> error $ "Lss: Error parsing file " ++ f ++ ": " ++ err
                                Right state -> return $ mappend s state)
              mempty
              files
  addConfig heist mempty { hcInterpretedSplices = lssSplices st }
  return Lss


lssSplices :: (MonadIO m, Functor m) => LssState -> Splices (Splice m)
lssSplices st = "lss" ## lssSplice
  where lssSplice =
          do n <- getParamNode
             case X.getAttribute "apply" n of
               Nothing -> error "Lss: Can't apply <lss> tag without `apply` attribute."
               Just a ->
                 case parseApp a of
                   Left err -> error $ "Lss: error parsing `apply` attribute: " ++ err
                   Right (LssApp ident args) ->
                     do case apply st ident args  of
                          Left err -> error $ "Lss: error applying " ++ (show $ unIdent ident)
                                           ++ ": " ++ show err
                          Right rules -> do
                            sym <- fmap (("lss" ++) . show . hashUnique) $ liftIO newUnique
                            return $ attach sym rules (X.elementChildren n)
