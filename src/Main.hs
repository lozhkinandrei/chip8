module Main where

import           Config
import           Control.Exception  (throw)
import           Control.Monad      (when)
import           Data.Bits
import qualified Data.ByteString    as B
import           Data.Text          (pack)
import qualified Data.Vector        as V
import qualified Data.Word          as W
import           Emulator
import           Fonts              (fonts)
import           Foreign.C.Types    (CInt)
import           Keyboard           (keypadMap)
import qualified Linear             as L
import           Linear.Affine      (Point (P))
import qualified SDL
import           SDL                (($=))
import           SDL.Video          (Rectangle (Rectangle), Renderer,
                                     WindowConfig (windowInitialSize), clear,
                                     createRenderer, createWindow,
                                     defaultRenderer, defaultWindow, drawLine,
                                     drawRect, fillRect, present,
                                     rendererDrawColor)
import           System.Environment (getArgs)
import qualified System.Random      as R



main :: IO ()
main = do
    -- initialize SDL
    SDL.initializeAll

    -- get args
    args <- getArgs
    let romFilePath = if null args then "roms/test-1-chip8-logo.ch8" else head args

    -- read ROM file
    romData <- readRom romFilePath

    -- init emulator (load ROM and fonts)
    let emulator = loadFont . loadRom romData . initEmulator $ seed

    -- init renderer
    renderer <- initRenderer

    -- get current tick
    tick <- SDL.ticks

    -- start recursive loop
    loop 0 renderer tick emulator

    SDL.quit

initEmulator :: Int ->  Emulator
initEmulator seed = Emulator {
    memory = V.replicate 4096 0,
    registers = V.replicate 16 0,
    delayTimer = 0,
    soundTimer = 0,
    i = 0,
    pc = 0x200,
    stack = [],
    display = V.replicate (fromIntegral $ width * height) False,
    waitingForKeyDepress = Nothing,
    pseudoRandomNumberGenerator = R.mkStdGen seed,
    keypad = V.replicate 16 False
}

closeWindowPressed ::[SDL.EventPayload] -> Bool
closeWindowPressed events = SDL.QuitEvent `elem` events

readRom :: FilePath -> IO [W.Word8]
readRom filePath  = B.unpack <$> B.readFile filePath

loadRom :: [W.Word8] -> Emulator -> Emulator
loadRom romData emulator = emulator { memory = V.update (memory emulator) (V.zip (V.fromList [0x200..]) (V.fromList romData))}

loadFont :: Emulator -> Emulator
loadFont emulator = emulator { memory = V.update (memory emulator) (V.zip (V.fromList [0x000..]) fonts)}

initRenderer :: IO Renderer
initRenderer = do
    let windowConfig = defaultWindow { windowInitialSize = L.V2 (width * scale) (height * scale) }
    -- Create Window
    window <- createWindow (pack "Chip-8 Emulator") windowConfig
    -- Create Renderer
    renderer <- createRenderer window (-1) defaultRenderer
    -- Change color to black
    rendererDrawColor renderer $= pixelColor
    -- Fill the screen with black
    clear renderer
    -- Show the window
    present renderer

    return renderer

fetchOpcode :: Emulator -> W.Word16
fetchOpcode emulator = firstNibble `shiftL` 8 .|. secondNibble
    where
        progmemoryCounter = fromIntegral $ pc emulator
        firstNibble = fromIntegral (memory emulator V.! progmemoryCounter) :: W.Word16
        secondNibble = fromIntegral (memory emulator V.! (progmemoryCounter + 1)) :: W.Word16

updateKeypad :: (SDL.Scancode -> Bool) -> Emulator -> Emulator
updateKeypad keyIsPressed emulator = emulator {keypad = V.map keyIsPressed keypadMap}


updateTimers :: Emulator -> Emulator
updateTimers emulator = emulator {delayTimer = if delayTimer emulator > 0 then delayTimer emulator - 1 else 0, soundTimer = if soundTimer emulator > 0 then soundTimer emulator - 1 else 0}

loop :: Int -> Renderer -> W.Word32 -> Emulator -> IO ()
loop operations renderer prevTick emulator = do
    events <- fmap (map SDL.eventPayload) SDL.pollEvents
    keys <- SDL.getKeyboardState

    -- quit if close window button was pressed
    when (closeWindowPressed events) $ throw $ userError "Quit button pressed."

    -- 1. update emulator with new keypad state
    let emulator' = updateKeypad keys emulator
    -- 2. fetch opcode
    let opcode = fetchOpcode emulator'
    -- 3. execute opcode
    let emulator'' = executeOpcode opcode emulator'

    currentTick <- SDL.ticks

    let timeElapsed = fromIntegral $ currentTick - prevTick

    if timeElapsed < interval then do
        when (operations > operationsToProcess) $ SDL.delay (fromIntegral $ interval - timeElapsed)
        loop (operations + 1) renderer prevTick emulator''

    else
        do
            renderDisplay renderer (display emulator'')
            let emulator''' = updateTimers emulator''
            tick <- SDL.ticks
            loop 0 renderer tick emulator'''

clearScreen :: Emulator -> Emulator
clearScreen emulator = emulator { display = V.replicate (fromIntegral $ width * height) False}

setRegister :: V.Vector W.Word8 -> W.Word8 -> W.Word8 -> V.Vector W.Word8
setRegister _registers register value = V.update _registers (V.fromList [(fromIntegral register, value)])

addRegister :: V.Vector W.Word8 -> W.Word8 -> W.Word8 -> V.Vector W.Word8
addRegister _registers register value = V.update _registers (V.fromList [(fromIntegral register, _registers V.! fromIntegral register + value)])

-- extract four hex (4-bit) values from opcode (16-bit),
-- there is no W.Word4 type available in Haskell so we use W.Word8 instead
patternMatchOpcode :: W.Word16 -> (W.Word8, W.Word8, W.Word8, W.Word8)
patternMatchOpcode  opcode = (
    fromIntegral $ (opcode .&. 0xF000) `shiftR` 12,
    fromIntegral $ (opcode .&. 0x0F00) `shiftR` 8,
    fromIntegral $ (opcode .&. 0x00F0) `shiftR` 4,
    fromIntegral $ opcode .&. 0x000F
    )

spriteAsBits :: V.Vector W.Word8 -> [Bool]
spriteAsBits = concatMap (\byte -> reverse [testBit byte i | i <- [0..7]])

getSpriteCoordsOnDisplay :: W.Word8 -> W.Word8 -> W.Word8 -> [Int]
getSpriteCoordsOnDisplay x y spriteHeight  = [fromIntegral row * fromIntegral width + fromIntegral col | row <- [y..y+spriteHeight-1], col <- [x..x+7]]

hasCollision :: V.Vector Bool -> V.Vector Bool -> Bool
hasCollision prevDisplay newDisplay  = or $ V.map (\(v1, v2) -> v1  && not v2 ) (V.zip prevDisplay newDisplay)

drawSprite :: Emulator -> W.Word8 -> W.Word8 -> W.Word8 -> (V.Vector Bool, Bool)
drawSprite emulator x y n = do
    let display' = display emulator
    let sprite = V.slice (fromIntegral $ i emulator) (fromIntegral n) (memory emulator)
    let vx = fromIntegral ((registers emulator V.! fromIntegral x) `mod` 64)
    let vy = fromIntegral ((registers emulator V.! fromIntegral y) `mod` 32)


    let spriteCoords = V.fromList $ getSpriteCoordsOnDisplay vx vy n
    let spriteBits = V.fromList $ spriteAsBits sprite


    let processedSpriteData = V.map (\(index, bit) -> do
            let wrappedIndex = index `mod` (64 * 32)
            let wrapped = index /= wrappedIndex

            let prevDisplayPixel = display' V.! wrappedIndex
            let newDisplayPixel = if clipping && wrapped then prevDisplayPixel else prevDisplayPixel `xor` bit

            (wrappedIndex, newDisplayPixel)
            ) (V.zip spriteCoords spriteBits)

    let newDisplay = V.update display' processedSpriteData
    let collision = hasCollision display' newDisplay

    (newDisplay, collision)


incrementPC :: Emulator -> Emulator
incrementPC emulator = emulator {pc = pc emulator + 2}

executeOpcode :: W.Word16 -> Emulator -> Emulator
executeOpcode opcode emulator = case patternMatchOpcode opcode of
    (0x0, 0x0, 0xE, 0x0) -> incrementPC $ emulator {display = V.replicate (fromIntegral $ width * height) False}
    (0x0, 0x0, 0xE, 0xE) -> emulator {pc = head (stack emulator), stack = tail (stack emulator)}
    (0x0, _, _, _) -> emulator {pc = fromIntegral $ opcode .&. 0x0FFF}
    (0x1, _, _, _) -> emulator {pc = fromIntegral $ opcode .&. 0x0FFF}
    (0x2, _, _, _) -> emulator {stack =  (pc emulator + 2) : stack emulator, pc = opcode .&. 0x0FFF}
    (0x3, x, _, _) -> emulator {pc = pc emulator + increment} where
        increment = if registers emulator V.! fromIntegral x == fromIntegral opcode .&. 0x00FF then 4 else 2
    (0x4, x, _, _) -> emulator {pc = pc emulator + increment} where
        increment = if registers emulator V.! fromIntegral x /= fromIntegral opcode .&. 0x00FF then 4 else 2
    (0x5, x, y, 0x0) -> emulator {pc = pc emulator + increment} where
        increment = if registers emulator V.! fromIntegral x == registers emulator V.! fromIntegral y then 4 else 2
    (0x6, x, _, _) -> incrementPC $ emulator {registers = setRegister (registers emulator) x (fromIntegral opcode .&. 0x00FF)}
    (0x7, x, _, _) -> incrementPC $ emulator {registers = addRegister (registers emulator) x (fromIntegral opcode .&. 0x00FF)}
    (0x8, x, y, 0x0) -> incrementPC $ emulator {registers = setRegister (registers emulator) x (registers emulator V.! fromIntegral y)}
    (0x8, x, y, 0x1) -> incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x (registers emulator V.! fromIntegral x .|. registers emulator V.! fromIntegral y))   0xF 0}
    (0x8, x, y, 0x2) -> incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x (registers emulator V.! fromIntegral x .&. registers emulator V.! fromIntegral y))    0xF 0}
    (0x8, x, y, 0x3) -> incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x (registers emulator V.! fromIntegral x `xor` registers emulator V.! fromIntegral y))  0xF 0}
    (0x8, x, y, 0x4) -> do
        let sum = fromIntegral (registers emulator V.! fromIntegral x) + fromIntegral (registers emulator V.! fromIntegral y) :: Int
        let vf = if sum > 255 then 1 else 0
        incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x (fromIntegral sum)) 0xF vf}
    (0x8, x, y, 0x5) -> do
        let diff = fromIntegral (registers emulator V.! fromIntegral x) - fromIntegral (registers emulator V.! fromIntegral y) :: Int
        let vf = if diff < 0 then 0 else 1
        incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x (fromIntegral diff)) 0xF vf}
    -- check about the quirks
    (0x8, x, y, 0x6) -> do
        let vy = registers emulator V.! fromIntegral y
        let vf = vy .&. 1
        let vx = vy `shiftR` 1
        incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x vx) 0xF vf}
    (0x8, x, y, 0x7) -> do
        let diff = fromIntegral (registers emulator V.! fromIntegral y) - fromIntegral (registers emulator V.! fromIntegral x) :: Int
        let vf = if diff < 0 then 0 else 1
        incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x (fromIntegral diff)) 0xF vf}
    -- check about the quirks
    (0x8, x, y, 0xE) -> do
        let vy = registers emulator V.! fromIntegral y
        let vf = if vy .&. 0x80 == 0x80 then 1 else 0
        let vx = vy `shiftL` 1
        incrementPC $ emulator {registers = setRegister (setRegister (registers emulator) x vx) 0xF vf}
    (0x9, x, y, 0x0) -> emulator {pc = pc emulator + increment} where
        increment = if registers emulator V.! fromIntegral x /= registers emulator V.! fromIntegral y then 4 else 2
    (0xA, _, _, _) -> incrementPC $ emulator {i = opcode .&. 0x0FFF}
    (0xB, _, _, _) -> emulator {pc = fromIntegral (opcode .&. 0x0FFF) + fromIntegral (registers emulator V.! 0)}
    (0xC, x, _, _) -> do
        let (number, generator) = R.randomR (0, 255) (pseudoRandomNumberGenerator emulator)
        incrementPC $ emulator {registers = setRegister (registers emulator) x (number .&. fromIntegral (opcode .&. 0x00FF)), pseudoRandomNumberGenerator = generator}
    (0xD, x, y, n) -> do
        let (newDisplay, collision) = drawSprite emulator x y n
        let vf = if collision then 1 else 0
        incrementPC $ emulator {display = newDisplay, registers = setRegister (registers emulator) 0xF vf}
    (0xE, x, 0x9, 0xE) -> do
        let key = fromIntegral $ registers emulator V.! fromIntegral x
        let isKeyPressed = keypad emulator V.! key
        emulator {pc = pc emulator + if isKeyPressed then 4 else 2}
    (0xE, x, 0xA, 0x1) -> do
        let key = fromIntegral $ registers emulator V.! fromIntegral x
        let isKeyPressed = keypad emulator V.! key
        emulator {pc = pc emulator + if not isKeyPressed then 4 else 2}
    (0xF, x, 0x0, 0x7) -> incrementPC $ emulator {registers = setRegister (registers emulator) x (delayTimer emulator)}
    (0xF, x, 0x0, 0xA) -> do
        case waitingForKeyDepress emulator of
            Just key -> do
                let depressed = not $ keypad emulator V.! fromIntegral key

                if depressed then incrementPC $ emulator {registers = setRegister (registers emulator) x (fromIntegral key), waitingForKeyDepress = Nothing}
                else emulator

            Nothing -> do
                let key = V.findIndex id (keypad emulator)
                case key of
                    Just key -> emulator {waitingForKeyDepress = Just (fromIntegral key)}
                    Nothing -> emulator
    (0xF, x, 0x1, 0x5) -> incrementPC $ emulator {delayTimer = registers emulator V.! fromIntegral x}
    (0xF, x, 0x1, 0x8) -> incrementPC $ emulator {soundTimer = registers emulator V.! fromIntegral x}
    (0xF, x, 0x1, 0xE) -> incrementPC $ emulator {i = i emulator + fromIntegral (registers emulator V.! fromIntegral x)}
    (0xF, x, 0x2, 0x9) -> incrementPC $ emulator {i = 5 * fromIntegral (registers emulator V.! fromIntegral x)} -- multiplied by 5 because each sprite is 5 bytes long
    (0xF, x, 0x3, 0x3) -> incrementPC $ emulator {memory = V.update (memory emulator) (V.fromList [(fromIntegral (i emulator), (registers emulator V.! fromIntegral x) `div` 100), (fromIntegral (i emulator + 1), (registers emulator V.! fromIntegral x) `div` 10 `mod` 10), (fromIntegral (i emulator + 2), (registers emulator V.! fromIntegral x) `mod` 10)])}
    (0xF, x, 0x5, 0x5) -> incrementPC $ emulator {memory = V.update (memory emulator) (V.zip (V.fromList [fromIntegral (i emulator)..]) (V.take (fromIntegral x + 1) (V.fromList (V.toList (registers emulator))))), i = i emulator + fromIntegral (registers emulator V.! fromIntegral x) + 1}
    (0xF, x, 0x6, 0x5) -> incrementPC $ emulator {registers = V.update (registers emulator) (V.fromList [(fromIntegral i, fromIntegral val) | (i, val) <- zip [0..fromIntegral x] (V.toList (V.slice (fromIntegral $ i emulator) (fromIntegral x + 1) (memory emulator)))]), i = i emulator + fromIntegral (registers emulator V.! fromIntegral x) + 1 }
    _ -> throw $ userError "Unknown opcode"

renderDisplay :: Renderer -> V.Vector Bool -> IO ()
renderDisplay renderer display = do
    -- Change color to black
    rendererDrawColor renderer $= backgroundColor
    -- Fill the screen with black
    clear renderer
    -- Change color to pixel
    rendererDrawColor renderer $= pixelColor
    -- Draw pixels
    mapM_ (\(i,v) -> when v $ drawPixel (i `mod` width) (i `div` width) renderer
        ) (zip [0..] (V.toList display))

    -- Change color to pixel
    rendererDrawColor renderer $= gridColor

    -- Draw horizontal lines of the grid
    mapM_ (\x -> SDL.Video.drawLine renderer (P (L.V2 (x * scale) 0)) (P (L.V2 (x * scale) (height * scale)))
        ) [x | x <- [1..width * scale], fromIntegral x * scale `mod` fromIntegral scale == 0]

    -- Draw vertical lines of the grid
    mapM_ (\y -> SDL.Video.drawLine renderer (P (L.V2 0 (y * scale))) (P (L.V2 ( width * scale) (y * scale)))
        ) [y | y <- [1..height * scale], fromIntegral y * scale `mod` fromIntegral scale == 0]

    present renderer

drawPixel :: CInt -> CInt -> Renderer -> IO ()
drawPixel x y renderer = do
    SDL.Video.drawRect renderer (Just (SDL.Video.Rectangle (P $ L.V2 (x * scale)  (y * scale)) (L.V2 scale scale)))
    SDL.Video.fillRect renderer (Just (SDL.Video.Rectangle (P $ L.V2 (x * scale)  (y * scale)) (L.V2 scale scale)))
