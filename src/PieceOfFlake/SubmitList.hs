module PieceOfFlake.SubmitList where


import PieceOfFlake.CmdArgs
    ( SubmitListOfFlakesArgs(indexTimeoutIn, webServiceUrl) )
import PieceOfFlake.Prelude as P
import PieceOfFlake.Flake
    ( FlakeUrl(unFlakeUrl), Flake(flakeUrl), RawFlakeUrl(RawFlakeUrl) )
import PieceOfFlake.Req
    ( POST(POST),
      ReqBodyJson(ReqBodyJson),
      defaultHttpConfig,
      jsonResponse,
      responseBody,
      runReq,
      dynReq,
      (/:),
      bsResponse,
      GET(GET),
      NoReqBody(NoReqBody),
      dynReq' )
import System.IO (hPutChar, hPutStrLn)
import UnliftIO.Exception ( catchIO )
data IndexTimeout = IndexTimeout deriving (Show, Eq)
instance Exception IndexTimeout

runSubmitList :: PoF m => SubmitListOfFlakesArgs -> m ()
runSubmitList sla = liftIO go
  where
    htp = runReq defaultHttpConfig
    waitUntilIndexed startedAt fu = do
      br <- htp $ dynReq' GET sla.webServiceUrl (\ur -> ur /: "flake" /: "status" /: unFlakeUrl fu) NoReqBody bsResponse
      case responseBody br of
        "Indexed" -> pr $ "OK " <> unFlakeUrl fu
        "Bad" -> pr $ "BAD " <> unFlakeUrl fu
        "NotFound" -> pr $ "WSER " <> unFlakeUrl fu
        _o -> do
          _ <- hPutChar stderr '.'
          P.hFlush stderr
          threadDelay (1 :: Second)
          now <- getTimeAfter startedAt
          if now `diffUTCTime` startedAt > untag sla.indexTimeoutIn then
            pr $ "TIMEOUT " <> unFlakeUrl fu
          else
            waitUntilIndexed startedAt fu
    erLn = void (hPutChar stderr '\n')
    pr msg = void (erLn >> putTextLn msg)
    go = catchIO getLine (\_e -> pure "") >>= \case
      "" -> pure ()
      l -> do
        _ <- hPutStrLn stderr $ "Indexing " <> toString l
        let rjb = ReqBodyJson $ RawFlakeUrl l
        jr <- htp $ dynReq POST sla.webServiceUrl "submit-flake" rjb jsonResponse
        case responseBody jr of
          Nothing -> do
            pr $ "FAIL-TO-SUBMIT " <> l
            go
          Just (f :: Flake) -> do
            now <- getCurrentTime
            waitUntilIndexed now f.flakeUrl
            go
