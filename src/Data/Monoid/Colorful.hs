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
-- <https://github.com/minad/colorful-monoids/blob/master/example.hs example.hs> file.
-----------------------------------------------------------

module Data.Monoid.Colorful (
  -- * The Monoid
  Colored(..)
  , Style(..)
  , Color(..)

  -- * Terminal type
  , Term(..)
  , hGetTerm
  , getTerm

  -- ** Colorful printing to file handle
  , hPrintColored
  , printColored
  , hPrintColoredIO
  , printColoredIO
  , hPrintColoredS
  , printColoredS

  -- ** Show with ANSI escape sequences
  , showColored
  , showColoredA
  , showColoredS

  -- * Reexport from Data.Semigroup
  , (<>)
) where

import Data.Monoid.Colorful.Nested
import Data.Semigroup ((<>))
