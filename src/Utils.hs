module Utils where

import qualified Data.Word   as W
import           Text.Printf (printf)


convertHexToString :: W.Word16 -> String
convertHexToString = printf "%04x"
