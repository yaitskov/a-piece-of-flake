{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module PieceOfFlake.Yesod where

import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as M
import Data.Text (intercalate)
import GHC.Records ( HasField )
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header ( hAcceptEncoding )
import Network.Socket (SockAddr(SockAddrInet), hostAddressToTuple, tupleToHostAddress, HostAddress)
import Network.Wai ( Request(remoteHost) )
import PieceOfFlake.CmdArgs ( StaticCacheSeconds )
import PieceOfFlake.Prelude
import Text.Blaze ( ToMarkup(toMarkup) )
import Text.Show qualified as TS
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import Yesod.Core
    ( hamlet,
      getYesod,
      cacheSeconds,
      widgetToPageContent,
      typeSvg,
      getMessages,
      sendResponseStatus,
      waiRequest,
      withUrlRenderer,
      Html,
      Yesod,
      ToContent(..),
      ToTypedContent(..),
      Content(ContentBuilder),
      HandlerFor,
      PageContent(pageBody, pageTitle, pageDescription, pageHead),
      TypedContent(TypedContent),
      WidgetFor, lookupHeader, addHeader )


newtype HostIp = HostIp HostAddress deriving newtype (Eq, Ord)
instance Show HostIp where
  show = toString . hostIpToDec
instance IsString HostIp where
  fromString s =
    fromMaybe (error . toText $ "Failed to parse [" <> s <> "] as IPv4 address") $ parseIpV4 s

newtype ClientAdr = ClientAdr HostIp deriving (Eq, Ord)

instance Show ClientAdr where
  show = show . clientAdrToDec4

clientAdrToDec4 :: ClientAdr -> Text
clientAdrToDec4 (ClientAdr sa) = hostIpToDec sa

getClientAdr :: HandlerFor a ClientAdr
getClientAdr =
  waiRequest >>= \r ->
    case remoteHost r of
      SockAddrInet _port hip -> pure . ClientAdr $ HostIp hip
      _ -> throwIO $ stringException "Unsupported socket addr"

parseIpV4 :: String -> Maybe HostIp
parseIpV4 s =
  case getAllTextSubmatches (s =~ ipPat) of
    [_full, a, b, c, d] ->
      case readEither a of
        Left _ -> Nothing
        Right ai ->
          case readEither b of
            Left _ -> Nothing
            Right bi ->
              case readEither c of
                Left _ -> Nothing
                Right ci ->
                  case readEither d of
                    Left _ -> Nothing
                    Right di ->
                      pure . HostIp $ tupleToHostAddress (ai, bi, ci, di)
    _ -> Nothing
  where
    ipPat :: String = "([[:digit:]]+)[.]([[:digit:]]+)[.]([[:digit:]]+)[.]([[:digit:]]+)"

hostIpToDec :: HostIp -> Text
hostIpToDec (HostIp hip) =
  case hostAddressToTuple hip of
    (a, b, c, d) -> intercalate "." $ fmap show [a, b, c, d]


internalError :: Text -> HandlerFor y a
internalError =
  sendResponseStatus H.status503


data Contentable = forall x. (ToContent x, ToTypedContent x) => Contentable x
instance ToContent Contentable where
  toContent (Contentable x) = toContent x

instance ToTypedContent Contentable where
  toTypedContent (Contentable x) = toTypedContent x

newtype FavIcon = FavIcon ByteString

instance ToContent FavIcon where
  toContent (FavIcon bs) =
    ContentBuilder (fromByteString bs) (Just . fromIntegral $ BS.length bs)
instance ToTypedContent FavIcon where
  toTypedContent = TypedContent typeSvg . toContent

mp3Mime :: Mime
mp3Mime = Mime "audio/mpeg"

bulmaLayout :: Yesod site => WidgetFor site () -> HandlerFor site Html
bulmaLayout w = do
  p <- widgetToPageContent w
  msgs <- getMessages
  withUrlRenderer
    [hamlet|
      $newline never
      $doctype 5
      <html>
          <head>
              <title>#{pageTitle p}
              $maybe description <- pageDescription p
                <meta name="description" content="#{description}">
              ^{pageHead p}
          <body>
              $forall (status, msg) <- msgs
                  <p class="message #{status}">#{msg}
              ^{pageBody p}
      |]

newtype Ts = Ts { unTs :: UtcBox } deriving newtype (Show, Eq, Ord)

instance ToMarkup Ts where
  toMarkup = toMarkup . show @Text . unTs

type HasCacheField y = (HasField "staticCache" y (Tagged StaticCacheSeconds Word32), Yesod y)
setCacheHeaderForStatic :: HasCacheField y => HandlerFor y ()
setCacheHeaderForStatic = do
  y <- getYesod
  cacheSeconds . fromIntegral $ untag y.staticCache

newtype Mime = Mime ByteString

sendStaticBs :: (HasCacheField y, ToContent a) => Mime -> a -> HandlerFor y TypedContent
sendStaticBs (Mime mime) c = do
  setCacheHeaderForStatic
  pure . TypedContent mime $ toContent c

data ContentEncoding = Gzip | Br deriving (Show, Eq, Ord)

contentEncodingToHeaderValue :: ContentEncoding -> Text
contentEncodingToHeaderValue = \case
  Gzip -> "gzip"
  Br -> "br"

-- to customize Accpet-Encoding in Firefox:
-- open tab about:config  -> network.http.accept-encoding.secure
parseContentEncoding :: Monad m => ByteString -> (Maybe ContentEncoding -> m a) -> m a
parseContentEncoding ce cb = go Nothing "" ce
  where
    go bestMatch en bs =
      case B8.uncons bs of
        Nothing ->
          case en of
            "gzip" -> cb $ bestMatch <|> pure Gzip
            "br" -> cb $ pure Br
            _ -> cb bestMatch
        Just (',', bs') ->
          case en of
            "gzip" -> go (Just Gzip) "" (B8.drop 1 bs')
            "br" -> cb $ pure Br
            _ -> go bestMatch "" (B8.drop 1 bs')
        Just (c, bs') ->
          go bestMatch (en `B8.snoc` c) bs'

staticFile :: (ToContent a, HasCacheField y) => Mime -> a -> Map ContentEncoding a -> HandlerFor y TypedContent
staticFile mime plainContent preEncodedContent = do
  lookupHeader hAcceptEncoding >>= \case
    Nothing -> sendStaticBs mime plainContent
    Just ae -> parseContentEncoding ae $ \case
      Nothing -> sendStaticBs mime plainContent
      Just ce ->
        case M.lookup ce preEncodedContent of
          Nothing -> sendStaticBs mime plainContent
          Just cnt -> do
            addHeader "Content-Encoding" $ contentEncodingToHeaderValue ce
            sendStaticBs mime cnt
  -- addHeader :: MonadHandler m => Text -> Text -> m ()
  -- replaceOrAddHeader :: MonadHandler m => Text -> Text -> m ()
  -- lookupHeader :: MonadHandler m => CI ByteString -> m (Maybe ByteString)
  -- get supported encodings
  -- pick best
  -- set Content-Encoding header
