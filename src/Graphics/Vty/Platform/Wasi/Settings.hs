{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Runtime settings for @vty-unix@. Most applications will not need to
-- change any of these settings.
module Graphics.Vty.Platform.Wasi.Settings
  ( VtyUnixConfigurationError(..)
  , UnixSettings(..)
  , currentTerminalName
  , defaultSettings
  )
where

import Control.Exception (Exception(..), throwIO)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Typeable (Typeable)
import System.Environment (lookupEnv)

-- | Type of exceptions that can be raised when configuring Vty on a
-- Unix system.
data VtyUnixConfigurationError =
    MissingTermEnvVar
    -- ^ The @TERM@ environment variable is not set.
    deriving (Show, Eq, Typeable)

instance Exception VtyUnixConfigurationError where
    displayException MissingTermEnvVar = "TERM environment variable not set"

-- | Runtime library settings for interacting with Unix terminals.
--
-- See this page for details on @VTIME@ and @VMIN@:
--
-- http://unixwiz.net/techtips/termios-vmin-vtime.html
data UnixSettings =
    UnixSettings { settingPtyName :: String
                 -- ^ Identifier of the pty under globalThis.vty-wasi
                 , settingTermName :: String
                 -- ^ The terminal name used to look up terminfo capabilities.
                 }
                 deriving (Show, Eq)

-- | Default runtime settings used by the library.
defaultSettings :: IO UnixSettings
defaultSettings = do
    mb <- lookupEnv termVariable
    case mb of
      Nothing -> throwIO MissingTermEnvVar
      Just t -> return $ UnixSettings { settingPtyName = "pty", settingTermName  = t }

termVariable :: String
termVariable = "TERM"

currentTerminalName :: IO (Maybe String)
currentTerminalName = lookupEnv termVariable
