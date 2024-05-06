module Config where

import qualified Data.Word       as W
import           Foreign.C.Types (CInt)
import qualified Linear          as L


seed :: Int
seed = 100

width :: CInt
width = 64

height :: CInt
height = 32

scale :: CInt
scale = 16

fps :: Int
fps = 60

interval :: Int
interval = 1000 `div` fps

-- how many operations per frame, if fps is 60 and 8 operations per frame, then total number of operations per second would be 60 * 8 = 480
operationsToProcess :: Int
operationsToProcess = 8

backgroundColor :: L.V4 W.Word8
backgroundColor = L.V4 25 25 25 255

pixelColor :: L.V4 W.Word8
pixelColor = L.V4 255 255 255 255

gridColor :: L.V4 W.Word8
gridColor = L.V4 5 5 5 255

clipping :: Bool
clipping = True
