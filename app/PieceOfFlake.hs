module Main where

import PieceOfFlake.CmdArgs
import PieceOfFlake.CmdRun
import PieceOfFlake.Prelude

main :: IO ()
main = execWithArgs runCmd
