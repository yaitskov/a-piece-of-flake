{-# LANGUAGE DuplicateRecordFields #-}
module PieceOfFlake.Flake where

import Data.Aeson ( FromJSONKey, ToJSONKey, encode )
import Data.Map.Strict qualified as M
import Data.SafeCopy ( deriveSafeCopy, base )
import Data.Text qualified as T
import PieceOfFlake.Prelude hiding (Map)
import Text.Blaze ( ToMarkup )
import Yesod.Core
    ( typeJson,
      ToContent(..),
      ToTypedContent(..),
      TypedContent(TypedContent), PathPiece )

newtype RawFlakeUrl = RawFlakeUrl Text
  deriving newtype (Show, FromJSON)

newtype FlakeUrl = FlakeUrl Text
  deriving newtype (Show, Read, Eq, Ord, ToMarkup, ToJSON, FromJSON,
                    Hashable, ToContent, ToTypedContent, IsString, ToText, PathPiece)

deriveSafeCopy 1 'base ''FlakeUrl

instance ToContent (Maybe FlakeUrl) where
  toContent = toContent . encode
instance ToTypedContent (Maybe FlakeUrl) where
  toTypedContent = TypedContent typeJson . toContent

instance ToContent [FlakeUrl] where
  toContent = toContent . encode
instance ToTypedContent [FlakeUrl] where
  toTypedContent = TypedContent typeJson . toContent

repoOfFlakeUrl :: FlakeUrl -> Text
repoOfFlakeUrl (FlakeUrl fu) = T.drop 1 $ T.dropWhile (/= '/') fu

newtype Architecture = Architecture Text
  deriving newtype
  ( Show, Eq, Ord, Hashable
  , ToJSON, FromJSON, ToJSONKey, FromJSONKey
  , IsString, ToText, ToMarkup
  )
deriveSafeCopy 1 'base ''Architecture
newtype PackageName = PackageName Text
  deriving newtype
  ( Show, Eq, Ord, Hashable
  , ToJSON, FromJSON, ToJSONKey, FromJSONKey
  , IsString, ToText, ToMarkup
  )
deriveSafeCopy 1 'base ''PackageName
data PackageInfo
  = PackageInfo
  { description :: Text
  , license :: Text
  , name :: PackageName
  , unfree :: Bool
  , platforms :: [ Text ]
  , broken :: Bool
  } deriving (Show, Eq, Generic)
deriveSafeCopy 1 'base ''PackageInfo
instance ToJSON PackageInfo
instance FromJSON PackageInfo

data MetaFlake
  = MetaFlake
  { description :: Maybe Text
  , packages :: M.Map Architecture (M.Map PackageName PackageInfo)
  , hasNixOsModules :: Bool
  , rev :: Text
  , flakeDeps :: [ FlakeUrl ]
  } deriving (Show, Eq, Generic)
deriveSafeCopy 1 'base ''MetaFlake

instance ToJSON MetaFlake
instance FromJSON MetaFlake

newtype IpAdr = IpAdr Text  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToMarkup)
deriveSafeCopy 1 'base ''IpAdr
newtype FetcherId = FetcherId Text deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToMarkup, Read)
deriveSafeCopy 1 'base ''FetcherId

data Flake
  = SubmittedFlake
  { flakeUrl :: FlakeUrl
  , submittedAt :: UtcBox
  , submittedFrom :: IpAdr
  }
  | FlakeIsBeingFetched
  { flakeUrl :: FlakeUrl
  , submitionFetchedAt :: UtcBox
  , fetcherId :: FetcherId
  }
  | BadFlake
  { flakeUrl :: FlakeUrl
  , fetcherRespondedAt :: UtcBox
  , error :: Text
  }
  | FlakeFetched
  { flakeUrl :: FlakeUrl
  , uploadedAt :: UtcBox
  , meta :: MetaFlake
  }
  | FlakeIndexed
  { flakeUrl :: FlakeUrl
  , indexedAt :: UtcBox
  , meta :: MetaFlake
  }
  deriving (Show, Eq, Generic)

deriveSafeCopy 1 'base ''Flake
instance ToJSON Flake
instance FromJSON Flake

instance ToContent Flake where
  toContent = toContent . encode
instance ToTypedContent Flake where
  toTypedContent = TypedContent typeJson . toContent

isIndexed :: Flake -> Bool
isIndexed FlakeIndexed {} = True
isIndexed _ = False

linkUrl :: FlakeUrl -> Text
linkUrl (FlakeUrl (T.stripPrefix "github:" -> Just s)) =
  "https://github.com/" <> s
linkUrl (FlakeUrl s) =
  "#not-gh-link-" <> s
