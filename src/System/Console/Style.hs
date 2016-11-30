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
-- keeps track of a stack of the active styles using a state monad.
-- This makes it easy to use this library for a pretty printer with
-- nested annotations, e.g., wl-pprint-console.
--
-- Warning: Windows support is currently not implemented, but
-- is planned (by using ansi-terminal or the ffi).
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
-- For many more examples, see the
-- <https://raw.githubusercontent.com/minad/console-style/master/Example.hs Example.hs> file.
-----------------------------------------------------------

module System.Console.Style (
  Color(..),
  HasStyle(..),
  SetStyle(..),
  Style,
  Term(..),
  defaultStyle,
  hDefaultStyle,
  hGetTerm,
  hRunStyle,
  hRunWithStyle,
  runStyle,
  runStyleT,
  runWithStyle,
  setStyle,
  setStyleCode,
  withStyle,
  changeStyle,
  applyStyle,
  applyStyleCode,
) where

import Data.Foldable (toList)
import Data.Word
import Data.Bool (bool)
import Data.List (intercalate, isPrefixOf, isInfixOf)
import Control.Monad.State.Strict
import System.IO (Handle, stdout, hPutStr, hIsTerminalDevice)
import System.Environment (getEnv)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

-- | Console color
data Color
  = DefaultColor                  -- ^ Default terminal color (terminal specific)
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
  | Color256 !Word8               -- ^ Color from 256 color scheme. Color is automatically reduced to 8 colors for less capable terminals.
  | RGB      !Word8 !Word8 !Word8 -- ^ True color. Color is automatically reduced to 256 or 8 colors for less capable terminals.
  deriving (Eq, Ord, Show)

-- | Terminal type. For less capable terminals the color depth is automatically reduced.
data Term
  = TermDumb -- ^ Dumb terminal - no color output
  | Term8    -- ^ 8 colors supported
  | Term256  -- ^ 256 colors supported
  | TermRGB  -- ^ True colors supported
  | TermWin  -- ^ Windows terminal. Will use emulation (Not yet implemented).
  deriving (Eq, Show)

-- | Style commands
data SetStyle
  = Bold           -- ^ Bold font
  | NotBold        -- ^ Normal-weight font
  | Italic         -- ^ Italic font
  | NotItalic      -- ^ Non-italic font
  | Under          -- ^ Underlined text
  | NotUnder       -- ^ Text without underline
  | Invert         -- ^ Invert foreground and background color
  | NotInvert      -- ^ Deactivate color inversion
  | Save           -- ^ Save style to stack
  | Restore        -- ^ Restore style from stack
  | Reset          -- ^ Reset to default style
  | Blink          -- ^ Activate blinking
  | NotBlink       -- ^ Deactivate blinking
  | FgColor !Color
  | BgColor !Color
  deriving (Eq, Ord, Show)

-- | State accessor for the 'Style'
class HasStyle s where
  getStyle :: s -> Style
  putStyle :: Style -> s -> s

instance HasStyle Style where
  getStyle = id
  putStyle = const

-- | Abstract state datatype which keeps
-- a stack of the applied styles.
data Style = Style
  { styleStack  :: !(NonEmpty StyleState)
  , styleActive :: !StyleState
  , styleHandle :: !Handle
  , styleTerm   :: !Term
  }

data StyleState = StyleState
  { styleBold   :: !Bool
  , styleItalic :: !Bool
  , styleUnder  :: !Bool
  , styleInvert :: !Bool
  , styleBlink  :: !Bool
  , styleFg     :: !Color
  , styleBg     :: !Color
  } deriving (Eq, Ord, Show)

-- | The action @(hGetTerm handle)@ determines the terminal type of the file @handle@.
--
-- The terminal type is determined by checking if the file handle points to a device
-- and by looking at the @$TERM@ environment variable.
hGetTerm :: MonadIO m => Handle -> m Term
hGetTerm h = liftIO $ do
  term <- hIsTerminalDevice h
  if term
    then envToTerm  <$> getEnv "TERM"
    else pure TermDumb

-- | Determine the terminal type from the value of the @$TERM@ environment variable.
-- TODO improve this
envToTerm :: String -> Term
envToTerm "dumb" = TermDumb
envToTerm term | any (flip isPrefixOf term) rgbTerminals = TermRGB
               | "256" `isInfixOf` term = Term256
               | otherwise = Term8
  where rgbTerminals = ["xterm", "konsole", "gnome", "st", "linux"]

-- | @(hDefaultStyle handle term) return the default (initial) 'Style' configured
-- with the file handle @handle@ and terminal type @term@.
--
-- Every 'Style' has a single associated handle.
hDefaultStyle :: Handle -> Term -> Style
hDefaultStyle h t = Style
  { styleStack  = pure defaultStyleState
  , styleHandle = h
  , styleTerm   = t
  , styleActive = defaultStyleState
  }

-- | The function @(defaultStyle term)@ returns the default 'Style' configured with terminal type @term@.
defaultStyle :: Term -> Style
defaultStyle = hDefaultStyle stdout

defaultStyleState :: StyleState
defaultStyleState = StyleState
  { styleBold   = False
  , styleItalic = False
  , styleInvert = False
  , styleUnder  = False
  , styleBlink  = False
  , styleFg     = DefaultColor
  , styleBg     = DefaultColor
  }

-- | The action @(hRunStyle handle action)@ runs the 'StateT' monad transformer providing
-- the active 'Style' for the given @action@.
hRunStyle :: MonadIO m => Handle -> StateT Style m a -> m a
hRunStyle h x = hDefaultStyle h <$> hGetTerm h >>= evalStateT x

-- | The action @(runStyle term action)@ runs the 'State' monad providing
-- the active 'Style' for the given @action@.
runStyle :: Term -> State Style a -> a
runStyle = flip evalState . defaultStyle

-- | The action @(runStyleT term action)@ runs the 'StateT' monad transformer providing
-- the active 'Style' for the given @action@.
runStyleT :: Monad m => Term -> StateT Style m a -> m a
runStyleT = flip evalStateT . defaultStyle

-- | The action @(runWithStyle cmd action)@ runs the 'StateT' monad transformer providing
-- the active 'Style' for the given @action@.
--
-- The output on 'stdout' within the @action@ is modified by the given 'StyleSet' commands @cmd@.
-- The 'Style' is restored to the defaults afterwards.
runWithStyle :: (MonadIO m, Foldable f) => f SetStyle -> StateT Style m a -> m a
runWithStyle = hRunWithStyle stdout

-- | The action @(hRunWithStyle handle cmd action)@ runs the 'StateT' monad transformer providing
-- the active 'Style' for the given @action@.
--
-- The output on @handle@ within the @action@ is modified by the given 'StyleSet' commands @cmd@.
-- The 'Style' is restored to the defaults afterwards.
hRunWithStyle :: (MonadIO m, Foldable f) => Handle -> f SetStyle -> StateT Style m a -> m a
hRunWithStyle h cmd action = hRunStyle h $ withStyle cmd action

-- | The action @(changeStyle cmd)@ modifies the active 'Style' by executing the 'StyleSet' commands @cmd@
-- without applying the changes.
--
-- You have to call applyStyle or applyStyleCode afterwards!
changeStyle :: (MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m ()
changeStyle cmd = do
  style <- gets getStyle
  modify $ putStyle $ new style
  where new style = style { styleStack = foldl go (styleStack style) cmd }
        go (_:|(x:xs)) Restore     = x :| xs
        go (_:|[])     Restore     = pure defaultStyleState
        go (x:|xs)     Save        = x :| (x:xs)
        go (_:|xs)     Reset       = defaultStyleState :| xs
        go (x:|xs)     Bold        = x { styleBold   = True  } :| xs
        go (x:|xs)     NotBold     = x { styleBold   = False } :| xs
        go (x:|xs)     Invert      = x { styleInvert = True  } :| xs
        go (x:|xs)     NotInvert   = x { styleInvert = False } :| xs
        go (x:|xs)     Italic      = x { styleItalic = True  } :| xs
        go (x:|xs)     NotItalic   = x { styleItalic = False } :| xs
        go (x:|xs)     Under       = x { styleUnder  = True  } :| xs
        go (x:|xs)     NotUnder    = x { styleUnder  = False } :| xs
        go (x:|xs)     Blink       = x { styleBlink  = True  } :| xs
        go (x:|xs)     NotBlink    = x { styleBlink  = False } :| xs
        go (x:|xs)     (BgColor c) = x { styleBg     = c     } :| xs
        go (x:|xs)     (FgColor c) = x { styleFg     = c     } :| xs

-- | The action @applyStyle@ applies the latest style changes.
applyStyle :: (MonadIO m, MonadState s m, HasStyle s) => m ()
applyStyle = do
  h <- gets (styleHandle . getStyle)
  applyStyleCode >>= liftIO . hPutStr h

-- | The action @applyStyleCode@ returns the ANSI code for the latest style changes.
applyStyleCode :: (MonadState s m, HasStyle s) => m String
applyStyleCode = do
  style <- gets getStyle
  let style' = style { styleActive = NonEmpty.head $ styleStack style }
  modify $ putStyle style'
  pure $ sgrCode (styleTerm style) (styleActive style) (styleActive style')

-- | The action @(styleCode cmd)@ returns the ANSI code corresponding to the 'StyleSet' commands @cmd@.
setStyleCode :: (MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m String
setStyleCode cmd = changeStyle cmd >> applyStyleCode

-- | The action @(setStyle cmd)@ modifies the active 'Style' by executing the 'StyleSet' commands @cmd@.
--
-- The style changes are applied immediately.
setStyle :: (MonadIO m, MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m ()
setStyle cmd = changeStyle cmd >> applyStyle

-- | The action @(withStyle cmd action)@ executes the @action@ with the active 'Style' modified
-- by the 'StyleSet' commands @cmd@.
--
-- The style is restored to the previous 'Style' afterwards.
withStyle :: (MonadIO m, MonadState s m, HasStyle s, Foldable f) => f SetStyle -> m a -> m a
withStyle cmd action = do
  setStyle (Save : toList cmd)
  ret <- action
  setStyle [Restore]
  pure ret

reduceColor :: Term -> Color -> Color
reduceColor Term8    = reduceColor8
reduceColor Term256  = reduceColor256
reduceColor _        = id

rgbToWord8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
rgbToWord8 base q r g b = base * (base * (r `div` q) + (g `div` q)) + (b `div` q)

gray24ToANSI :: Word8 -> Color
gray24ToANSI x
  | x < 6             = DullBlack
  | x >= 6 && x < 12  = Black
  | x >= 12 && x < 18 = DullWhite
  | otherwise         = White

color216ToANSI :: Word8 -> Color
color216ToANSI x = rgbToANSI 3 r g b
  where (r,gb) = divMod x 36
        (g,b)  = divMod gb 6

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
rgbToANSI q r g b = color16ToANSI $ bool 0 8 (squareNorm r g b >= squareNorm q q q) + rgbToWord8 2 q b g r

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

sgrCode :: Term -> StyleState -> StyleState -> String
sgrCode TermDumb _ _ = ""
sgrCode TermWin  _ _ = ""
sgrCode t old new
  | old == new = ""
  | new == defaultStyleState = csi 'm' []
  | otherwise = csi 'm' $
    flag   styleBlink  5  ++
    flag   styleBold   1  ++
    flag   styleItalic 3  ++
    flag   styleUnder  4  ++
    flag   styleInvert 7  ++
    color  styleFg     0  ++
    color  styleBg     10

  where

  update :: Eq a => (StyleState -> a) -> (a -> [Word8]) -> [Word8]
  update f g = let new' = f new in bool [] (g new') (new' /= f old)

  flag  f n = update f $ bool [20 + n] [n]
  color f n = update (reduceColor t . f) (\x -> let (c:|cs) = sgrColorArgs x in c + n : cs)

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
