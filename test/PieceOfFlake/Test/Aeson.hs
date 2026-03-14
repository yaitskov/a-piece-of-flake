{-# LANGUAGE MultilineStrings #-}
module PieceOfFlake.Test.Aeson where

import Data.Aeson ( decode )
import PieceOfFlake.Aeson ( Some(..) )
import PieceOfFlake.Prelude
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )


data A = A {b :: Bool, a :: Some Int } deriving (Show, Eq, Generic)
instance FromJSON A

test_e :: TestTree
test_e =
  testGroup "Aeson"
  [ testGroup "Some"
    [ go """{"b":true, "a":1}""" (Just (A True $ Atom 1))
    , go """{"b":false, "a":[2, 3]}""" (Just (A False $ More [2, 3]))
    ]
  ]
  where
    go s e = testCase (show s) $ decode s @?= e
