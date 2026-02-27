module PieceOfFlake.Fetcher where

import Data.Time.Clock ( getCurrentTime )
import PieceOfFlake.Req
    ( defaultHttpConfig,
      jsonResponse,
      responseBody,
      runReq,
      MonadHttp,
      POST(POST),
      ReqBodyJson(ReqBodyJson),
      DynamicUrl,
      dynReq )
import PieceOfFlake.Flake
    ( Flake(error, BadFlake, flakeUrl, fetcherRespondedAt),
      FlakeUrl(..) )
import PieceOfFlake.Prelude


uploadFlakeAndFetch ::  (MonadHttp m, MonadIO m) => DynamicUrl -> Maybe Flake -> m (Maybe FlakeUrl)
uploadFlakeAndFetch surl f = do
  jr <- dynReq POST surl "fetch-new-flake-submitions" (ReqBodyJson f) jsonResponse
  case responseBody jr of
    Nothing -> uploadFlakeAndFetch surl Nothing
    Just fu@(FlakeUrl fu') -> do
      putStrLn $ "nix flake info --json " <> toString fu'
      putStrLn $ "nix eval --json " <> toString fu' <> ".#packages.x86_64-linux.... "
      now <- liftIO getCurrentTime
      uploadFlakeAndFetch surl . Just $
        BadFlake
        { flakeUrl = fu
        , fetcherRespondedAt = now
        , error = "Not implemented"
        }

runFetcher :: MonadIO m => DynamicUrl -> m ()
runFetcher serviceUrl =
  runReq defaultHttpConfig $ do
    void $ uploadFlakeAndFetch serviceUrl Nothing
