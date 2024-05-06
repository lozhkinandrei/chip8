module Emulator where

import qualified Data.Vector   as V
import qualified Data.Word     as W
import qualified System.Random as R



data Emulator = Emulator {
    display                     :: V.Vector Bool,
    keypad                      :: V.Vector Bool,
    memory                      :: V.Vector W.Word8,
    registers                   :: V.Vector W.Word8,
    i                           :: W.Word16,
    pc                          :: W.Word16,
    stack                       :: [W.Word16],
    delayTimer                  :: W.Word8,
    soundTimer                  :: W.Word8,
    waitingForKeyDepress        :: Maybe W.Word8,
    pseudoRandomNumberGenerator :: R.StdGen
} deriving (Show)



