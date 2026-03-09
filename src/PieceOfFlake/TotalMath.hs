module PieceOfFlake.TotalMath (Nz, notZero, divNz, realToFracNz) where

import Prelude

newtype Nz a = Nz a

notZero :: (Eq a, Num a) => a -> Maybe (Nz a)
notZero 0 = Nothing
notZero x = pure $ Nz x

divNz :: Fractional a => a -> Nz a -> a
divNz a (Nz b) = a / b

realToFracNz :: (Real a, Fractional b) => Nz a -> Nz b
realToFracNz (Nz x) = Nz $ realToFrac x
