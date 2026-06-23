{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Vty.Platform.Wasi.Pty
  ( Pty(..)
  , getWindowSize
  ) where

import GHC.IO.Device
import GHC.IO.Exception
import Data.Word
import Data.Coerce
import Control.Monad
import Control.Concurrent.MVar
import Foreign.Ptr
import Control.Concurrent.Async
import Graphics.Vty.Platform.Wasi.Pty.JSFFI
import Control.Exception
import Data.Primitive.ByteArray
import Foreign (allocaBytes)
import Data.Bits

data Termios = Termios
  { termios_iflag :: !Int
  , termios_oflag :: !Int
  , termios_cflag :: !Int
  , termios_lflag :: !Int
  , termios_cc    :: !ByteArray
  }

-- We don't need VTIME, VMIN because all bytes are guaranteed to be sent together.
pattern ICANON, ECHO :: Int
pattern ICANON = 0x0002
pattern ECHO = 0x0008

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

getTermios :: Pty -> IO Termios
getTermios pty = bracket
  (js_pty_tcgets pty)
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

setTermios :: Pty -> Termios -> IO ()
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

getWindowSize :: Pty -> IO (Int, Int)
getWindowSize pty = bracket
  (js_pty_winsize pty)
  (freeJSVal . coerce)
  \ws -> (,) <$> js_winsize_col ws <*> js_winsize_row ws

instance RawIO Pty where
  read pty ptr _ n = do
    readable <- js_pty_is_readable pty
    if readable
      then
        bracket
          (js_pty_read pty n)
          (freeJSVal . coerce)
          \jsarr -> copyJSByteArray jsarr ptr n
      else do
        readableMvar <- newEmptyMVar
        bracket
          (js_to_jscallable . void $ tryPutMVar readableMvar ())
          (freeJSVal . coerce)
          \cb ->
            bracket
              (js_pty_on_readable pty cb)
              (\dispose -> js_call_jsval dispose >> freeJSVal (coerce dispose))
              \_ -> takeMVar readableMvar
        bracket
          (js_pty_read pty n)
          (freeJSVal . coerce)
          \jsarr -> copyJSByteArray jsarr ptr n
 
  readNonBlocking pty ptr _ n = do
    bracket
      (js_pty_read pty n)
      (freeJSVal . coerce)
      \jsarr -> Just <$> copyJSByteArray jsarr ptr n

  write pty ptr _ n = do
    writable <- js_pty_is_writable pty
    if writable
      then do
        bracket
          (js_memory ptr n)
          (freeJSVal . coerce)
          \bytes -> js_pty_write pty bytes
      else do
        writableMvar <- newEmptyMVar
        bracket
          (js_to_jscallable . void $ tryPutMVar writableMvar ())
          (freeJSVal . coerce)
          \cb ->
            bracket
              (js_pty_on_writable pty cb)
              (\dispose -> js_call_jsval dispose >> freeJSVal (coerce dispose))
              \_ -> takeMVar writableMvar
        bracket
          (js_memory ptr n)
          (freeJSVal . coerce)
          \bytes -> js_pty_write pty bytes

  writeNonBlocking pty ptr _ n = do
    writable <- js_pty_is_writable pty
    if writable
      then do
        bracket
          (js_memory ptr n)
          (freeJSVal . coerce)
          \bytes -> js_pty_write pty bytes
        pure n
      else do
        pure 0

instance IODevice Pty where
  ready pty writing msecs = do
    let (isReady, onReady) =
          if writing
            then (js_pty_is_writable pty, js_pty_on_writable pty)
            else (js_pty_is_readable pty, js_pty_on_readable pty)
    r1 <- isReady
    if r1
      then pure True
      else do
        race_
          (js_sleep msecs)
          do
            readyMvar <- newEmptyMVar
            bracket
              (js_to_jscallable (void $ tryPutMVar readyMvar ()))
              (freeJSVal . coerce)
              \cb -> 
                bracket
                  (onReady cb)
                  (\dispose -> js_call_jsval dispose >> freeJSVal (coerce dispose))
                  \_ -> takeMVar readyMvar
        r2 <- isReady
        pure r2
  close _ = throwIO $ unsupportedOperation { ioe_location = "close @Pty" }
  isTerminal _ = pure True
  isSeekable _ = pure False
  seek _ _ _ = throwIO $ unsupportedOperation { ioe_location = "seek @Pty" }
  tell _ = throwIO $ unsupportedOperation { ioe_location = "tell @Pty" }
  getSize _ = pure (-1) -- behaviour of instances of stdout, stdin
  setSize _ _ = throwIO $ unsupportedOperation { ioe_location = "setSize @Pty" }
  setEcho pty echo = do
    ts <- getTermios pty
    let ts' = ts { termios_lflag = if echo then termios_lflag ts .|. ECHO else termios_lflag ts .&. complement ECHO }
    setTermios pty ts'
  getEcho pty = do
    ts <- getTermios pty
    pure $ termios_lflag ts .&. ECHO /= 0
  setRaw pty raw = do
    ts <- getTermios pty
    let ts' = ts { termios_lflag = if raw then termios_lflag ts .|. ICANON else termios_lflag ts .&. complement ICANON }
    setTermios pty ts'
  devType _ = pure Stream
  dup _ = throwIO $ unsupportedOperation { ioe_location = "dup @Pty" }
  dup2 _ _ = throwIO $ unsupportedOperation { ioe_location = "dup2 @Pty" }
