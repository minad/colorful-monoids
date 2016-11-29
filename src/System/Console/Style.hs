-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Style
-- Copyright   :  Daniel Mendler (c) 2016,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This library provides styled text output using ANSI
-- escape sequences. The main feature is that the library
-- keeps track of a stack of the applied styles using a state monad.
-- This makes it easy to use this library for a pretty printer with
-- nested annotations, e.g., wl-pprint-console.
--
-- Example:
--
-- > basicExample :: IO ()
-- > basicExample = runWithStyle [FgColor Blue] $ do
-- >   withStyle [Bold] $ liftIO $ putStr "Bold Blue"
-- >
-- >   setStyle [Save, Italic, BgColor Red]
-- >   liftIO $ putStr "Italic Red"
-- >   setStyle [Restore]
-- >
-- >   setStyle [Under]
-- >   liftIO $ putStr "Under Blue"
-- >   setStyle [Reset]
-- >
-- >   liftIO $ putStrLn "Normal output"
--
-- For many more examples, see the project's extensive
-- <https://raw.githubusercontent.com/minad/console-style/master/Example.hs Example.hs> file.
-----------------------------------------------------------

module System.Console.Style (
  Blink(..),
  Color(..),
  HasStyle(..),
  SetStyle(..),
  Style,
  StyleT,
  Term(..),
  defaultStyle,
  hDefaultStyle,
  hGetTerm,
  hRunStyle,
  hRunWithStyle,
  runStyle,
  runWithStyle,
  setStyle,
  styleCode',
  styleCode,
  withStyle,
) where

import Data.Foldable (toList)
import Data.Word
import Data.Bool (bool)
import Data.List (intercalate)
import Control.Monad.State.Strict
import System.IO (Handle, stdout, hPutStr, hIsTerminalDevice)
import System.Environment (getEnv)
import Data.List.NonEmpty (NonEmpty(..))

data Color
  = DefaultColor
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | DullBlack
  | DullRed
  | DullGreen
  | DullYellow
  | DullBlue
  | DullMagenta
  | DullCyan
  | DullWhite
  | Color256 !Word8
  | RGB      !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show)

data Blink = NoBlink | BlinkSlow | BlinkFast
  deriving (Eq, Ord, Show)

data Term = TermDumb | Term8 | Term256 | TermRGB | TermWin
  deriving (Eq, Show)

data SetStyle
  = Bold
  | NotBold
  | Italic
  | NotItalic
  | Under
  | NotUnder
  | Invert
  | NotInvert
  | Save
  | Restore
  | Reset
  | Blink   !Blink
  | FgColor !Color
  | BgColor !Color
  deriving (Eq, Ord, Show)

class HasStyle s where
  getStyle :: s -> Style
  putStyle :: Style -> s -> s

instance HasStyle Style where
  getStyle = id
  putStyle = const

data Style = Style
  { styleStack  :: !(NonEmpty StyleState)
  , styleHandle :: !Handle
  , styleTerm   :: !Term
  }

type StyleT = StateT Style

data StyleState = StyleState
  { styleBold   :: !Bool
  , styleItalic :: !Bool
  , styleUnder  :: !Bool
  , styleInvert :: !Bool
  , styleBlink  :: !Blink
  , styleFg     :: !Color
  , styleBg     :: !Color
  } deriving (Eq, Ord, Show)

hGetTerm :: MonadIO m => Handle -> m Term
hGetTerm h = liftIO $ do
  term <- hIsTerminalDevice h
  if term
    then envToTerm  <$> getEnv "TERM"
    else pure TermDumb

-- TODO improve this
envToTerm :: String -> Term
envToTerm "xterm" = TermRGB
envToTerm "dumb"  = TermDumb
envToTerm _       = TermRGB

mkDefaultStyle :: Handle -> Term -> Style
mkDefaultStyle h t = Style
  { styleStack  = pure defaultStyleState
  , styleHandle = h
  , styleTerm   = t
  }

hDefaultStyle :: MonadIO m => Handle -> m Style
hDefaultStyle h = mkDefaultStyle h <$> hGetTerm h

defaultStyle :: Term -> Style
defaultStyle = mkDefaultStyle stdout

defaultStyleState :: StyleState
defaultStyleState = StyleState
  { styleBold   = False
  , styleItalic = False
  , styleInvert = False
  , styleUnder  = False
  , styleBlink  = NoBlink
  , styleFg     = DefaultColor
  , styleBg     = DefaultColor
  }

hRunStyle :: MonadIO m => Handle -> StyleT m a -> m a
hRunStyle h x = hDefaultStyle h >>= evalStateT x

runStyle :: Term -> State Style a -> a
runStyle = flip evalState . defaultStyle

runWithStyle :: MonadIO m => [SetStyle] -> StyleT m a -> m a
runWithStyle = hRunWithStyle stdout

hRunWithStyle :: MonadIO m => Handle -> [SetStyle] -> StyleT m a -> m a
hRunWithStyle h style action = hRunStyle h $ withStyle style action

styleCode' :: Foldable f => Style -> f SetStyle -> (Style, String)
styleCode' style cmd = (style', sgrCode style style')
  where style' = updateStyle cmd style

styleCode :: (MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m String
styleCode cmd = do
  style <- gets getStyle
  let (style', str) = styleCode' style cmd
  modify $ putStyle style'
  pure str

setStyle :: (MonadIO m, MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m ()
setStyle cmd = do
  style <- gets getStyle
  let style' = updateStyle cmd style
  liftIO $ hPutStr (styleHandle style) $ sgrCode style style'
  modify $ putStyle style'

withStyle :: (MonadIO m, MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m a -> m a
withStyle cmd action = do
  setStyle (Save : toList cmd)
  ret <- action
  setStyle [Restore]
  pure ret

updateStyle :: Foldable f => f SetStyle -> Style -> Style
updateStyle cmd (Style stack h t) = Style (foldl go stack cmd) h t
  where go (_:|(x:xs)) Restore     = x :| xs
        go (_:|[])     Restore     = pure defaultStyleState
        go (x:|xs)     Save        = x :| (x:xs)
        go (_:|xs)     Reset       = defaultStyleState :| xs
        go (x:|xs)     Bold        = x { styleBold   = True  } :| xs
        go (x:|xs)     Invert      = x { styleInvert = True  } :| xs
        go (x:|xs)     Italic      = x { styleItalic = True  } :| xs
        go (x:|xs)     NotBold     = x { styleBold   = False } :| xs
        go (x:|xs)     NotInvert   = x { styleInvert = False } :| xs
        go (x:|xs)     NotItalic   = x { styleItalic = False } :| xs
        go (x:|xs)     NotUnder    = x { styleUnder  = False } :| xs
        go (x:|xs)     Under       = x { styleUnder  = True  } :| xs
        go (x:|xs)     (BgColor c) = x { styleBg     = c } :| xs
        go (x:|xs)     (Blink b)   = x { styleBlink  = b } :| xs
        go (x:|xs)     (FgColor c) = x { styleFg     = c } :| xs

reduceColor :: Term -> Color -> Color
reduceColor Term8    = reduceColor8
reduceColor Term256  = reduceColor256
reduceColor _        = id

rgbToWord8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
rgbToWord8 base q r g b = base * (base * (b `div` q) + (g `div` q)) + (r `div` q)

gray24ToANSI :: Word8 -> Color
gray24ToANSI x
  | x < 6             = DullBlack
  | x >= 6 && x < 12  = Black
  | x >= 12 && x < 18 = DullWhite
  | otherwise         = White

color216ToANSI :: Word8 -> Color
color216ToANSI x = rgbToANSI 3 r g b
  where (b,gr) = divMod x 36
        (g,r)  = divMod gr 6

color16ToANSI :: Word8 -> Color
color16ToANSI 0  = DullBlack
color16ToANSI 1  = DullRed
color16ToANSI 2  = DullGreen
color16ToANSI 3  = DullYellow
color16ToANSI 4  = DullBlue
color16ToANSI 5  = DullMagenta
color16ToANSI 6  = DullCyan
color16ToANSI 7  = DullWhite
color16ToANSI 8  = Black
color16ToANSI 9  = Red
color16ToANSI 10 = Green
color16ToANSI 11 = Yellow
color16ToANSI 12 = Blue
color16ToANSI 13 = Magenta
color16ToANSI 14 = Cyan
color16ToANSI _  = White

squareNorm :: Integral a => a -> a -> a -> a
squareNorm r g b = ri*ri + bi*bi * gi*gi
  where ri = fromIntegral r
        gi = fromIntegral g
        bi = fromIntegral b

rgbToANSI :: Word8 -> Word8 -> Word8 -> Word8 -> Color
rgbToANSI q r g b = color16ToANSI $ bool 0 8 (squareNorm r g b >= squareNorm q q q) + rgbToWord8 2 q r g b

reduceColor8 :: Color -> Color
reduceColor8 (Color256 x)
  | x < 16    = color16ToANSI x
  | x < 232   = color216ToANSI $ x - 16
  | otherwise = gray24ToANSI $ x - 232
reduceColor8 (RGB r g b) = rgbToANSI 128 r g b
reduceColor8 x = x

reduceColor256 :: Color -> Color
reduceColor256 (RGB r g b)
  | r == g && r == b = Color256 $ 232 + r `div` 11
  | otherwise = Color256 $ 16 + rgbToWord8 6 43 r g b
reduceColor256 x = x

csi :: Char -> [Word8] -> String
csi cmd args = "\ESC[" ++ intercalate ";" (map show args) ++ pure cmd

sgrCode :: Style -> Style -> String
sgrCode (Style _ _ TermDumb) _ = ""
sgrCode (Style _ _ TermWin)  _ = ""
sgrCode (Style (old:|_) _ t) (Style (new:|_) _ _)
  | old /= new && new == defaultStyleState = csi 'm' [0]
  | otherwise = csi 'm' $
    update styleBlink  (pure . sgrBlinkArg) ++
    flag   styleBold   1                    ++
    flag   styleItalic 3                    ++
    flag   styleUnder  4                    ++
    flag   styleInvert 7                    ++
    color  styleFg     0                    ++
    color  styleBg     10

  where

  update :: Eq a => (StyleState -> a) -> (a -> [Word8]) -> [Word8]
  update f g = let new' = f new in bool [] (g new') (new' /= f old)

  flag  f n = update f $ bool [20 + n] [n]
  color f n = update (reduceColor t . f) (\x -> let (c:|cs) = sgrColorArgs x in c + n : cs)

sgrBlinkArg :: Blink -> Word8
sgrBlinkArg NoBlink   = 25
sgrBlinkArg BlinkSlow = 5
sgrBlinkArg BlinkFast = 6

sgrColorArgs :: Color -> NonEmpty Word8
sgrColorArgs (Color256 n) = 38 :| [5, n]
sgrColorArgs (RGB r g b)  = 38 :| [2, r, g, b]
sgrColorArgs Black        = pure 90
sgrColorArgs Red          = pure 91
sgrColorArgs Green        = pure 92
sgrColorArgs Yellow       = pure 93
sgrColorArgs Blue         = pure 94
sgrColorArgs Magenta      = pure 95
sgrColorArgs Cyan         = pure 96
sgrColorArgs White        = pure 97
sgrColorArgs DullBlack    = pure 30
sgrColorArgs DullRed      = pure 31
sgrColorArgs DullGreen    = pure 32
sgrColorArgs DullYellow   = pure 33
sgrColorArgs DullBlue     = pure 34
sgrColorArgs DullMagenta  = pure 35
sgrColorArgs DullCyan     = pure 36
sgrColorArgs DullWhite    = pure 37
sgrColorArgs DefaultColor = pure 39
