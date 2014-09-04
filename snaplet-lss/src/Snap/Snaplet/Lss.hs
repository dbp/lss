{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Lss (Lss(..), initLss, lssSplices) where

import           Control.Monad      (when)
import           Data.List          (isSuffixOf)
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Unique
import           Heist              (Splices, getParamNode,
                                     hcInterpretedSplices, ( ## ))
import           Heist.Interpreted  (Splice)
import           Prelude            hiding ((++))
import           Snap
import           Snap.Snaplet.Heist (Heist, addConfig)
import           System.Directory
import           System.FilePath
import qualified Text.XmlHtml       as X

import           Language.Lss

(++) :: Monoid d => d -> d -> d
(++) = mappend

data Lss = Lss

initLss :: Snaplet (Heist b) -> SnapletInit b Lss
initLss heist = makeSnaplet "lss" "" Nothing $ do
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
                          Left err -> error $ "Lss: error applying " ++ (T.unpack $ unIdent ident)
                                           ++ ": " ++ T.unpack err
                          Right rules -> do
                            let childs = case X.getAttribute "class" n of
                                           Nothing -> X.elementChildren n
                                           Just cls -> map (addClass cls) (X.elementChildren n)
                            sym <- (("lss" ++) . show . hashUnique) <$> liftIO newUnique
                            return $ attach sym rules childs
          where addClass newCls n@(X.Element _ attrs _) =
                  let classAttr = case lookup "class" attrs  of
                                    Nothing -> ("class", newCls)
                                    Just cls -> ("class", newCls ++ " " ++ cls)
                  in n { X.elementAttrs = classAttr : filter ((/= "class").fst) attrs}
                addClass _ n = n
