{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Best-effort terminfo-based color mode detection.
--
-- This module is exposed for testing purposes only; applications should
-- never need to import this directly.
module Graphics.Vty.Platform.Wasi.Output.Color
  ( detectColorMode
  )
where

import System.Environment (lookupEnv)

import qualified System.Terminfo as Terminfo
import Data.Maybe

import Graphics.Vty.Attributes.Color
import System.Terminfo.Caps

detectColorMode :: String -> IO ColorMode
detectColorMode termName' = do
    term <- either (const $ Nothing) Just <$> Terminfo.acquireDatabase termName'
    let termColors = fromMaybe 0 $ term >>= (`Terminfo.queryNumTermCap` MaxColors)
    colorterm <- lookupEnv "COLORTERM"
    return $ if
        | termColors <  8               -> NoColor
        | termColors <  16              -> ColorMode8
        | termColors == 16              -> ColorMode16
        | termColors <  256             -> ColorMode240 (fromIntegral termColors - 16)
        | colorterm == Just "truecolor" -> FullColor
        | colorterm == Just "24bit"     -> FullColor
        | otherwise                     -> ColorMode240 240
