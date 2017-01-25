-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Colorful
-- Copyright   :  Daniel Mendler (c) 2017,
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
-- > basicExample = do
-- >   term <- getTerm
-- >   printColoredS term $ Style Under (Style Bold "Basic Example\n")
-- >     <> Style Bold "Bold"
-- >     <> Style Italic (Bg Red "Italic Red")
-- >     <> Style Under "Under"
-- >   putChar '\n'
--
-- For many more examples, see the
-- <https://github.com/minad/colored-monoids/blob/master/example.hs example.hs> file.
-----------------------------------------------------------

module Data.Monoid.Colorful (
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
  , (<>)
) where

import Data.Monoid.Colorful.Nested
import Data.Semigroup ((<>))
