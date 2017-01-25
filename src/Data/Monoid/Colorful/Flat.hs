-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Colorful.Flat
-- Copyright   :  Daniel Mendler (c) 2017,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the flat 'Colored' type,
-- which is used internally for rendering the
-- nested 'Data.Monoid.Colorful.Colored' but is
-- also useful on its own. The API resembles
-- the API of 'Data.Monoid.Colorful'.
--
-----------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Monoid.Colorful.Flat (
  Style(..)
  , Color(..)
  , Term(..)
  , Colored(..)
  , hGetTerm
  , getTerm
  , hPrintColored
  , printColored
  , hPrintColoredIO
  , printColoredIO
  , hPrintColoredS
  , printColoredS
  , showColored
  , showColoredA
  , showColoredS
) where

import System.IO (Handle, stdout, hPutStr)
import Data.Monoid.Colorful.Term
import Data.Monoid.Colorful.Settings
import Data.Monoid.Colorful.Color
import Data.Monoid.Colorful.SGR
import Data.Functor.Identity
import Data.Bifunctor (first, second)
import GHC.Generics (Generic, Generic1)

data Colored a
  = Value a
  | Style   !Style
  | Unstyle !Style
  | Fg      !Color
  | Bg      !Color
  | Push
  | Pop
  | Reset
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

hPrintColoredIO :: Handle -> Term -> [Colored (IO ())] -> IO ()
hPrintColoredIO h = showColored id (hPutStr h)

printColoredIO :: Term -> [Colored (IO ())] -> IO ()
printColoredIO = hPrintColoredIO stdout

hPrintColored :: (Handle -> a -> IO ()) -> Handle -> Term -> [Colored a] -> IO ()
hPrintColored f h = showColoredA (f h) (hPutStr h)

printColored :: (a -> IO ()) -> Term -> [Colored a] -> IO ()
printColored f = hPrintColored (const f) stdout

hPrintColoredS :: Handle -> Term -> [Colored String] -> IO ()
hPrintColoredS h = showColored (hPutStr h) (hPutStr h)

printColoredS :: Term -> [Colored String] -> IO ()
printColoredS = hPrintColoredS stdout

showColoredS :: Term -> [Colored String] -> ShowS
showColoredS = showColored (++) (++)

showColored :: Monoid o => (a -> o) -> (SGRCode -> o) -> Term -> [Colored a] -> o
showColored str code term flat = runIdentity $ showColoredA (pure . str) (pure . code) term flat

showColoredA :: (Applicative f, Monoid o) => (a -> f o) -> (SGRCode -> f o) -> Term -> [Colored a] -> f o
showColoredA str code term = go (defaultSettings, (defaultSettings, []))
  where go s (Style   a:b) = go ((second.first) (setStyle a True) s) b
        go s (Unstyle a:b) = go ((second.first) (setStyle a False) s) b
        go s (Fg      a:b) = go ((second.first) (setFg a) s) b
        go s (Bg      a:b) = go ((second.first) (setBg a) s) b
        go s (Push     :b) = go (second pushStack s) b
        go s (Pop      :b) = go (second popStack s) b
        go s (Reset    :b) = go (second resetStack s) b
        go s (Value   a:b) = let (old, stack@(new, _)) = s in
          mappend <$> (mappend <$> code (sgrCode term old new) <*> str a) <*> go (new, stack) b
        go s [] = let (old, (new, _)) = s in code (sgrCode term old new)
