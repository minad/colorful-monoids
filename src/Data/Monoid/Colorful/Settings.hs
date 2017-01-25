module Data.Monoid.Colorful.Settings (
  Settings(..)
  , defaultSettings
  , setStyle, setBg, setFg
  , resetStack, pushStack, popStack
) where

import Data.Monoid.Colorful.Color

data Settings = Settings
  { styleBold   :: !Bool
  , styleItalic :: !Bool
  , styleUnder  :: !Bool
  , styleInvert :: !Bool
  , styleBlink  :: !Bool
  , styleFg     :: !Color
  , styleBg     :: !Color
  } deriving (Eq)

type SettingsStack = (Settings, [Settings])

defaultSettings :: Settings
defaultSettings = Settings
  { styleBold   = False
  , styleItalic = False
  , styleInvert = False
  , styleUnder  = False
  , styleBlink  = False
  , styleFg     = DefaultColor
  , styleBg     = DefaultColor
  }

setStyle :: Style -> Bool -> Settings -> Settings
setStyle Bold   b s = s { styleBold   = b }
setStyle Italic b s = s { styleItalic = b }
setStyle Under  b s = s { styleUnder  = b }
setStyle Invert b s = s { styleInvert = b }
setStyle Blink  b s = s { styleBlink  = b }

setBg, setFg :: Color -> Settings -> Settings
setBg c s = s { styleBg = c }
setFg c s = s { styleFg = c }

resetStack, pushStack, popStack :: SettingsStack -> SettingsStack
resetStack (_, ys)     = (defaultSettings, ys)
pushStack  (y, ys)     = (y, y:ys)
popStack   (_, [])     = (defaultSettings, [])
popStack   (_, z : zs) = (z, zs)
