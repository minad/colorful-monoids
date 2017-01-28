module Data.Monoid.Colorful.SGR (
  SGRCode,
  sgrCode
) where

import Data.Monoid.Colorful.Term
import Data.Monoid.Colorful.Settings
import Data.Monoid.Colorful.Color
import Data.Word (Word8)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bool (bool)
import Data.List (intercalate)

type SGRCode = String

csi :: Char -> [Word8] -> SGRCode
csi cmd args = "\ESC[" ++ intercalate ";" (map show args) ++ pure cmd

sgrCode :: Term -> Settings -> Settings -> SGRCode
sgrCode TermDumb _ _ = ""
sgrCode TermWin  _ _ = ""
sgrCode t old new
  | old == new = ""
  | new == defaultSettings = csi 'm' []
  | otherwise = csi 'm' $
    flag  settingBlink  5  ++
    flag  settingBold   1  ++
    flag  settingItalic 3  ++
    flag  settingUnder  4  ++
    flag  settingInvert 7  ++
    color settingFg     0  ++
    color settingBg     10

  where

  update :: Eq a => (Settings -> a) -> (a -> [Word8]) -> [Word8]
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
