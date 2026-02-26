module PieceOfFlake.Th where

import AddDependentFile ( addDependentFile, getPackageRoot, (</>) )
import Data.FileEmbed ( embedFile )
import Language.Haskell.TH.Syntax ( Exp, Q )
import PieceOfFlake.Prelude ( (<$>), FilePath )

includeFile :: FilePath -> Q Exp
includeFile p = do
  ap <- (</> p) <$> getPackageRoot
  addDependentFile ap
  embedFile ap
