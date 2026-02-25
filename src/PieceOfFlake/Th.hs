module PieceOfFlake.Th where

import Data.FileEmbed ( embedFile )
import Language.Haskell.TH.Syntax ( Exp, Q, addDependentFile, makeRelativeToProject )
import PieceOfFlake.Prelude ( FilePath )

includeFile :: FilePath -> Q Exp
includeFile p = do
  ap <- makeRelativeToProject p
  addDependentFile ap
  embedFile ap
