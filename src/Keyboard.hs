module Keyboard where
import qualified Data.Vector as V
import qualified SDL


keypadMap :: V.Vector SDL.Scancode
keypadMap = V.fromList
    [
        SDL.ScancodeX, -- 0x0,
        SDL.Scancode1, -- 0x1,
        SDL.Scancode2, -- 0x2,
        SDL.Scancode3, -- 0x3,
        SDL.ScancodeQ, -- 0x4,
        SDL.ScancodeW, -- 0x5,
        SDL.ScancodeE, -- 0x6,
        SDL.ScancodeA, -- 0x7,
        SDL.ScancodeS, -- 0x8,
        SDL.ScancodeD, -- 0x9,
        SDL.ScancodeZ, -- 0xa,
        SDL.ScancodeC, -- 0xb,
        SDL.Scancode4, -- 0xc,
        SDL.ScancodeR, -- 0xd,
        SDL.ScancodeF, -- 0xe,
        SDL.ScancodeV  -- 0xf,
    ]
