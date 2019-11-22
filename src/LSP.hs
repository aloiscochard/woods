{-# LANGUAGE OverloadedStrings #-}

module LSP (diagnosticsLoop, initRepsonseFromRequest, definitionResponse, referencesResponse) where

import           Lens.Micro
import           Language.Haskell.LSP.Types.Capabilities hiding(_experimental, _colorProvider, _workspace)
import           Language.Haskell.LSP.Types as L
import           Language.Haskell.LSP.Types.Lens (range, params, diagnostics, uri)
import           Data.List  (groupBy, last)

diagnosticsLoop :: [Uri] -> [PublishDiagnosticsNotification] -> ([PublishDiagnosticsNotification], [Uri])
diagnosticsLoop store diagnostics = do
    let diagWithError = filterSameRangeDiagnostics <$> filter diagnosticErrorExist diagnostics
    let diagWithErrorUri = map uriFromPublishDiagnosticsNotification diagWithError
    let cleanedDiagnostic = filter (\e -> elem (uriFromPublishDiagnosticsNotification e) store && notElem (uriFromPublishDiagnosticsNotification e) diagWithErrorUri) diagnostics
    let notCleanedYet = filter (\e -> e `notElem` map uriFromPublishDiagnosticsNotification cleanedDiagnostic) store
    let toSend = cleanedDiagnostic ++ diagWithError
    let newStore =notCleanedYet ++ diagWithErrorUri
    (toSend, newStore)


diagnosticErrorExist :: PublishDiagnosticsNotification -> Bool
diagnosticErrorExist publishDiagnosticsNotification = not $ null (publishDiagnosticsNotification ^. params . diagnostics)


uriFromPublishDiagnosticsNotification :: PublishDiagnosticsNotification -> Uri
uriFromPublishDiagnosticsNotification notification = notification ^. params . uri


filterSameRangeDiagnostics :: PublishDiagnosticsNotification -> PublishDiagnosticsNotification
filterSameRangeDiagnostics publishDiagnosticsNotification =
  publishDiagnosticsNotification & params . diagnostics  %~ uniqueDiagnositcsPosition
  where
    uniqueDiagnositcsPosition diagnostics =
      case diagnostics of
        List diags -> List $ last <$> groupBy (\d1 d2-> (d1 ^.range) == (d2 ^.range)) diags


initRepsonseFromRequest :: InitializeRequest -> InitializeResponse
initRepsonseFromRequest request = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $ InitializeResponseCapabilities serverCapabilities)
      Nothing


definitionResponse :: DefinitionRequest -> Maybe L.Location -> DefinitionResponse
definitionResponse request maybeLocation = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $
        case maybeLocation of
          Nothing -> MultiLoc []
          Just loc -> SingleLoc loc)
      Nothing

referencesResponse :: ReferencesRequest -> [L.Location] -> ReferencesResponse
referencesResponse request locations = case request of
  (RequestMessage _ origId _ _) ->
    ResponseMessage
      "2.0"
      (responseId origId)
      (Just $ List locations)
      Nothing


-- No serverCapabilities at all for now
serverCapabilities =
  InitializeResponseCapabilitiesInner
    { _textDocumentSync                 = Nothing
    , _hoverProvider                    = Just False
    , _completionProvider               = Nothing
    , _signatureHelpProvider            = Nothing
    , _definitionProvider               = Just True
    , _typeDefinitionProvider           = Nothing
    , _implementationProvider           = Nothing
    , _referencesProvider               = Just True
    , _documentHighlightProvider        = Just False
    , _documentSymbolProvider           = Just False
    , _workspaceSymbolProvider          = Just False
    , _codeActionProvider               = Just False
    , _codeLensProvider                 = Nothing
    , _documentFormattingProvider       = Just False
    , _documentRangeFormattingProvider  = Just False
    , _documentOnTypeFormattingProvider = Nothing
    , _renameProvider                   = Just False
    , _documentLinkProvider             = Nothing
    , _colorProvider                    = Nothing
    , _foldingRangeProvider             = Nothing
    , _executeCommandProvider           = Nothing
    , _workspace                        = Nothing
    , _experimental                     = Nothing
    }
