{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Vty.Platform.Wasi.Pty
  ( getWindowSize
  , Termios(..)
  , getTermios
  , setTermios
  , pattern ICANON
  , pattern ISIG
  , pattern ECHO
  , pattern IEXTEN
  , pattern ICRNL
  , pattern IXON
  ) where

import Data.Word
import Data.Coerce
import Foreign.Ptr
import Graphics.Vty.Platform.Wasi.Pty.JSFFI
import Control.Exception
import Data.Primitive.ByteArray
import Foreign (allocaBytes)
import System.Posix (Fd(..))

data Termios = Termios
  { termios_iflag :: !Int
  , termios_oflag :: !Int
  , termios_cflag :: !Int
  , termios_lflag :: !Int
  , termios_cc    :: !ByteArray
  }

-- We don't need VTIME, VMIN because all bytes are guaranteed to be sent together.
-- They are also not supported on xterm-pty.
pattern ICANON, ECHO, ICRNL, IXON, ISIG, IEXTEN :: Int

pattern ICRNL = 0x0100
pattern IXON = 0x0400

pattern ISIG = 0x0001
pattern ICANON = 0x0002
pattern ECHO = 0x0008
pattern IEXTEN = 0x8000

jsTypeOf :: JSVal -> IO String
jsTypeOf jsv = bracket
  (js_typeof jsv)
  (freeJSVal . coerce)
  (\s -> pure $! fromJSString s)

withJSString :: String -> (JSString -> IO r) -> IO r
withJSString str k = bracket
  (pure $ toJSString str)
  (freeJSVal . coerce)
  k

onWindowChange :: Fd -> (IO ()) -> IO (IO ())
onWindowChange (Fd fd) cb = undefined

-- | Copies at most 'n' bytes of the JSByteArray to the given
-- location in memory starting from index 0.
-- Returns the actual number of bytes copied.
copyJSByteArray :: JSByteArray -> Ptr Word8 -> Int -> IO Int
copyJSByteArray jsarr ptr n = do
  len <- max 0 . min n <$> js_byte_array_length jsarr
  bracket
    (js_subarray jsarr 0 len)
    (freeJSVal . coerce)
    \jsarr' -> do
      bracket
        (js_memory ptr len)
        (freeJSVal . coerce)
        \mem -> do
          js_copy_to_byte_array jsarr' mem
          pure len

getTermios :: Fd -> IO Termios
getTermios (Fd fd) = bracket
  (js_fd_ioctl_tcgets fd)
  (freeJSVal . coerce)
  \jts -> bracket
    (js_termios_cc jts)
    (freeJSVal . coerce)
    \jcc -> do
      len <- js_byte_array_length jcc
      ba <- newByteArray len
      allocaBytes len \ptr -> do
        bracket
          (js_memory ptr len)
          (freeJSVal . coerce)
          \mem -> js_copy_to_byte_array jcc mem
        copyPtrToMutableByteArray ba 0 ptr len 
      ba' <- unsafeFreezeByteArray ba
      Termios <$> js_termios_iflag jts <*> js_termios_oflag jts <*> js_termios_cflag jts <*> js_termios_lflag jts <*> pure ba'

setTermios :: Fd -> Termios -> IO ()
setTermios pty Termios{..} = do
  let len = sizeofByteArray termios_cc
  allocaBytes len \ptr -> do
    copyByteArrayToPtr ptr termios_cc 0 len
    bracket
      (js_memory ptr len)
      (freeJSVal . coerce)
      \jsba -> bracket
        (js_to_termios termios_iflag termios_oflag termios_cflag termios_lflag jsba)
        (freeJSVal . coerce)
        \jts -> js_pty_tcsets pty jts

getWindowSize :: Fd -> IO (Int, Int)
getWindowSize pty = bracket
  (js_pty_winsize pty)
  (freeJSVal . coerce)
  \ws -> (,) <$> js_winsize_col ws <*> js_winsize_row ws
