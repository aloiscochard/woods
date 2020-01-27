{-# LANGUAGE OverloadedStrings #-}

module CTags  where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Lens.Micro
import qualified Language.Haskell.LSP.Types.Lens as LSPLens
import qualified Language.Haskell.LSP.Types as L
import Language.Haskell.LSP.Types (DefinitionRequest, DefinitionResponse)
import System.Directory (getCurrentDirectory)
import System.Process (readProcess)

-- "This is pure Haskell code!" Obi-Wan Kenobi

ctagsLocationFromRequest :: DefinitionRequest -> IO ([L.Location])
ctagsLocationFromRequest definitionRequest = do
  currentDirectory <- getCurrentDirectory
  res <- readProcess "tags-lsp" ([BSLC.unpack $ JSON.encode definitionRequest, currentDirectory ++ "/tags"]) ""
  case JSON.decode (BSLC.pack res) :: Maybe DefinitionResponse of
    Nothing ->
      return []
    Just response -> do
      case response^. LSPLens.result of
        Nothing  ->
          return []
        Just locations  -> do
          case locations of
            L.SingleLoc location -> return [location]
            L.MultiLoc locations -> return locations
