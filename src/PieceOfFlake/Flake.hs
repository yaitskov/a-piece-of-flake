{-# LANGUAGE DuplicateRecordFields #-}
module PieceOfFlake.Flake where

import Data.Aeson ( FromJSONKey, ToJSONKey, encode )
import Data.Map.Strict qualified as M
import Data.SafeCopy ( deriveSafeCopy, base )
import PieceOfFlake.Prelude hiding (Map)
import Yesod.Core
    ( FromJSON,
      ToJSON,
      typeJson,
      ToContent(..),
      ToTypedContent(..),
      TypedContent(TypedContent) )


newtype FlakeUrl = FlakeUrl Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Hashable, ToContent, ToTypedContent, IsString, ToText)

deriveSafeCopy 1 'base ''FlakeUrl

instance ToContent (Maybe FlakeUrl) where
  toContent = toContent . encode
instance ToTypedContent (Maybe FlakeUrl) where
  toTypedContent = TypedContent typeJson . toContent

instance ToContent [FlakeUrl] where
  toContent = toContent . encode
instance ToTypedContent [FlakeUrl] where
  toTypedContent = TypedContent typeJson . toContent
newtype Architecture = Architecture Text
  deriving newtype
  ( Show, Eq, Ord, Hashable
  , ToJSON, FromJSON, ToJSONKey, FromJSONKey
  , IsString, ToText
  )
deriveSafeCopy 1 'base ''Architecture
newtype PackageName = PackageName Text
  deriving newtype
  ( Show, Eq, Ord, Hashable
  , ToJSON, FromJSON, ToJSONKey, FromJSONKey
  , IsString, ToText
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
  , rev :: Text
  , flakeDeps :: [ FlakeUrl ]
  } deriving (Show, Eq, Generic)
deriveSafeCopy 1 'base ''MetaFlake

instance ToJSON MetaFlake
instance FromJSON MetaFlake

newtype IpAdr = IpAdr Text  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)
deriveSafeCopy 1 'base ''IpAdr
newtype FetcherId = FetcherId Text deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)
deriveSafeCopy 1 'base ''FetcherId

data Flake
  = SubmittedFlake
  { flakeUrl :: FlakeUrl
  , submittedAt :: UTCTime
  , submittedFrom :: IpAdr
  }
  | FlakeIsBeingFetched
  { flakeUrl :: FlakeUrl
  , submitionFetchedAt :: UTCTime
  , fetcherId :: FetcherId
  }
  | BadFlake
  { flakeUrl :: FlakeUrl
  , fetcherRespondedAt :: UTCTime
  , error :: Text
  }
  | FlakeFetched
  { flakeUrl :: FlakeUrl
  , uploadedAt :: UTCTime
  , meta :: MetaFlake
  }
  | FlakeIndexed
  { flakeUrl :: FlakeUrl
  , indexedAt :: UTCTime
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
