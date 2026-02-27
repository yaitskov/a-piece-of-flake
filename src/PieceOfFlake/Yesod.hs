module PieceOfFlake.Yesod where

import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.Text (intercalate)
import Network.HTTP.Types qualified as H
import Network.Socket (SockAddr(SockAddrInet), hostAddressToTuple, tupleToHostAddress, HostAddress)
import Network.Wai ( Request(remoteHost) )
import PieceOfFlake.Prelude
import Text.Show qualified as TS
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import UnliftIO.Exception ( stringException, throwIO )
import Yesod.Core -- ( HandlerFor, waiRequest, sendResponseStatus )

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
