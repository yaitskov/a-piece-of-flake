module PieceOfFlake.Aeson
  ( module Data.Aeson
  , Some (..)
  , someToList
  ) where

import Data.Aeson
    ( Value(Array, Number, String, Bool, Object, Null),
      FromJSON(parseJSON) )
import PieceOfFlake.Prelude ( Eq, Show, (<$>) )

data Some x = Atom x | More [x] deriving (Show, Eq)

instance FromJSON x => FromJSON (Some x) where
  parseJSON n@Number {} = Atom <$> parseJSON n
  parseJSON n@String {} = Atom <$> parseJSON n
  parseJSON n@Bool {} = Atom <$> parseJSON n
  parseJSON n@Object {} = Atom <$> parseJSON n
  parseJSON n@Null {} = Atom <$> parseJSON n
  parseJSON n@Array {} = More <$> parseJSON n

someToList :: Some a -> [a]
someToList = \case
  Atom x -> [x]
  More xs -> xs
