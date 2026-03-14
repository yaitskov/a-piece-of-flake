{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Req
 ( module PieceOfFlake.Req
 , module Network.HTTP.Req
 )
where

import Network.HTTP.Req
import PieceOfFlake.Prelude as P
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import Text.Show as S

urlRegex :: Text
urlRegex = "^(http|https)://([a-z0-9._-]+)(:([1-9][0-9]*))?/?$"

data DynamicUrl
  = UrlHttp
    { duu :: Url 'Http
    , duo :: Option 'Http
    }
  | UrlHttps
    { duus :: Url 'Https
    , duos :: Option 'Https
    }

instance Show DynamicUrl where
  show (UrlHttp ur o) = "http://" <> S.show ur <> S.show (queryParamToList o)
  show (UrlHttps ur o) = "https://" <> S.show ur <> S.show (queryParamToList o)


secToTimeout :: Second -> Option a
secToTimeout = responseTimeout . fromIntegral . toMicroseconds

setResponseTimeout :: DynamicUrl -> Second ->  DynamicUrl
setResponseTimeout a@UrlHttp {} s = a { duo = a.duo <> secToTimeout s }
setResponseTimeout a@UrlHttps {} s = a { duos = a.duos <> secToTimeout s }

parseUrl :: Text -> Either Text DynamicUrl
parseUrl ur =
  case getAllTextSubmatches (ur =~ urlRegex) of
    ([_full, protocolG, domainG ] :: [Text]) ->
      case protocolG of
        "http" -> pure $ UrlHttp (http domainG) mempty
        "https" -> pure $ UrlHttps (https domainG) mempty
        _ -> Left $ "Bad protocol: " <> protocolG
    ([_full, protocolG, domainG, _colon, portG ] :: [Text]) ->
      case readMaybe $ toString portG of
        Nothing -> Left $ "Bad port " <> portG
        Just p
          | p > 0 && p < 65123 ->
            case protocolG of
              "http" -> pure . UrlHttp (http domainG) $ port p
              "https" -> pure . UrlHttps (https domainG) $ port p
              _ -> P.error $ "Bad protocol: " <> protocolG
          | otherwise ->
            Left $ "Bad port: " <> P.show p
    _badUrl -> Left $ "Bad service url: " <> P.show ur

dynReq :: forall m method body response.
  ( MonadHttp m
  , HttpMethod method
  , HttpBody body
  , HttpResponse response
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
  method -> DynamicUrl -> Text -> body -> Proxy response -> m response
dynReq m dUrl path = dynReq' m dUrl (/: path)

dynReq' :: forall m method body response.
  ( MonadHttp m
  , HttpMethod method
  , HttpBody body
  , HttpResponse response
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
  method -> DynamicUrl -> (forall u. Url u -> Url u) -> body -> Proxy response -> m response
dynReq' m dUrl pathF body pro =
  case dUrl of
    UrlHttp ur o -> req m (pathF ur) body pro o
    UrlHttps ur o -> req m (pathF ur) body pro o
