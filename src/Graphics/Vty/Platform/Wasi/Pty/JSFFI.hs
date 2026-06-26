{-# LANGUAGE CPP #-}
#if defined(wasi_HOST_OS)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

{-# LANGUAGE EmptyCase #-}

module Graphics.Vty.Platform.Wasi.Pty.JSFFI
  ( JSByteArray(..)
  , Pty(..)
  , JSTermios(..)
  , JSWinsize(..)
  , JSCallable(..)
  , JSObject(..)
  , js_byte_array_length
  , js_subarray
  , js_memory
  , js_copy_to_byte_array
  , js_get_global
  , js_index_object
  , js_typeof
  , js_instanceof
  , js_pty_write
  , js_pty_read
  , js_pty_on_readable
  , js_pty_is_readable
  , js_pty_on_writable
  , js_pty_is_writable
  , js_pty_on_signal
  , js_call_jsval
  , js_to_jscallable
  , js_sleep
  , js_pty_tcgets
  , js_pty_tcsets
  , js_termios_iflag
  , js_termios_oflag
  , js_termios_cflag
  , js_termios_lflag
  , js_termios_cc
  , js_to_termios
  , js_pty_winsize
  , js_winsize_col
  , js_winsize_row

  , JSVal
  , freeJSVal
  , JSString(..)
  , fromJSString
  , toJSString
  )
  where

#if defined(wasi_HOST_OS)
import GHC.Wasm.Prim
#endif
import Data.Word
import Foreign.Ptr


newtype JSByteArray = JSByteArray JSVal

-- | Based on slave half of openpty() from https://github.com/mame/xterm-pty
newtype Pty = Pty JSVal

newtype JSTermios = JSTermios JSVal

newtype JSWinsize = JSWinsize JSVal

newtype JSCallable = JSCallable JSVal

newtype JSObject = JSObject JSVal

#if defined(wasi_HOST_OS)

foreign import javascript unsafe "$1.length"
  js_byte_array_length :: JSByteArray -> IO Int

foreign import javascript unsafe "$1.subarray($2, $3)"
  js_subarray :: JSByteArray -> Int -> Int -> IO JSByteArray

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer.slice($2, $2 + $3))"
  js_memory :: Ptr Word8 -> Int -> IO JSByteArray

-- assumption: doing the loop entirely within js is faster
foreign import javascript unsafe "for (i=0; i<$1.length; i++) { $2[i] = $1[i] }"
  js_copy_to_byte_array :: JSByteArray -> JSByteArray -> IO ()


foreign import javascript unsafe "globalThis[$2]"
  js_get_global :: JSString -> IO JSVal

foreign import javascript unsafe "$1[$2]"
  js_index_object :: JSObject -> JSString -> IO JSVal

foreign import javascript unsafe "typeof $1"
  js_typeof :: JSVal -> IO JSString

foreign import javascript unsafe "$1 instanceof $2"
  js_instanceof :: JSVal -> JSObject -> IO Bool


foreign import javascript unsafe "$1.write(Array.from($2))"
  js_pty_write :: Pty -> JSByteArray -> IO ()

foreign import javascript unsafe "Uint8Array.from($1.read($2))"
  js_pty_read :: Pty -> Int -> IO JSByteArray

foreign import javascript unsafe "$1.onReadable($2).dispose"
  js_pty_on_readable :: Pty -> JSCallable -> IO JSCallable

foreign import javascript unsafe "$1.readable"
  js_pty_is_readable :: Pty -> IO Bool

foreign import javascript unsafe "$1.onWritable($2).dispose"
  js_pty_on_writable :: Pty -> JSCallable -> IO JSCallable

foreign import javascript unsafe "$1.writable"
  js_pty_is_writable :: Pty -> IO Bool

foreign import javascript unsafe "$1.onSignal(sig => { switch (sig) { case 'SIGINT': return $2(); case 'SIGQUIT': return $3(); case 'SIGTSTP': return $4(); case 'SIGWINCH': return $5(); } }).dispose"
  js_pty_on_signal :: Pty -> JSCallable -> JSCallable -> JSCallable -> JSCallable -> IO JSCallable


foreign import javascript unsafe "dynamic"
  js_call_jsval :: JSCallable -> IO ()

foreign import javascript "wrapper"
  js_to_jscallable :: IO () -> IO JSCallable

foreign import javascript safe "new Promise(res => setTimeout(res, $1))"
  js_sleep :: Int -> IO ()


foreign import javascript unsafe "$1.ioctl('TCGETS')"
  js_pty_tcgets :: Pty -> IO JSTermios

foreign import javascript unsafe "$1.ioctl('TCSETS', $2)"
  js_pty_tcsets :: Pty -> JSTermios -> IO ()

foreign import javascript unsafe "$1.iflag"
  js_termios_iflag :: JSTermios -> IO Int

foreign import javascript unsafe "$1.oflag"
  js_termios_oflag :: JSTermios -> IO Int

foreign import javascript unsafe "$1.cflag"
  js_termios_cflag :: JSTermios -> IO Int

foreign import javascript unsafe "$1.lflag"
  js_termios_lflag :: JSTermios -> IO Int

foreign import javascript unsafe "new Uint8Array.from($1.cc)"
  js_termios_cc    :: JSTermios -> IO JSByteArray

foreign import javascript unsafe "{iflag: $1, oflag: $2, cflag: $3, lflag: $4, cc: Array.from($5)}"
  js_to_termios    :: Int -> Int -> Int -> Int -> JSByteArray -> IO JSTermios


foreign import javascript unsafe "$1.ioctl('TIOCGWINSZ')"
  js_pty_winsize :: Pty -> IO JSWinsize

foreign import javascript unsafe "$1[0]"
  js_winsize_col :: JSWinsize -> IO Int

foreign import javascript unsafe "$1[1]"
  js_winsize_row :: JSWinsize -> IO Int

#else
-- stub version of the module for HLS

data JSVal

freeJSVal :: JSVal -> IO ()
freeJSVal v = case v of {}


newtype JSString = JSString JSVal

fromJSString :: JSString -> String 
fromJSString = noJsffi

toJSString :: String -> JSString
toJSString = noJsffi


noJsffi :: a
noJsffi = error "no JSFFI"


js_byte_array_length :: JSByteArray -> IO Int
js_byte_array_length = noJsffi

js_subarray :: JSByteArray -> Int -> Int -> IO JSByteArray
js_subarray = noJsffi

js_memory :: Ptr Word8 -> Int -> IO JSByteArray
js_memory = noJsffi

js_copy_to_byte_array :: JSByteArray -> JSByteArray -> IO ()
js_copy_to_byte_array = noJsffi


js_get_global :: JSString -> IO JSVal
js_get_global = noJsffi

js_index_object :: JSObject -> JSString -> IO JSVal
js_index_object = noJsffi

js_typeof :: JSVal -> IO JSString
js_typeof = noJsffi

js_instanceof :: JSVal -> JSObject -> IO Bool
js_instanceof = noJsffi


js_pty_write :: Pty -> JSByteArray -> IO ()
js_pty_write = noJsffi

js_pty_read :: Pty -> Int -> IO JSByteArray
js_pty_read = noJsffi

js_pty_on_readable :: Pty -> JSCallable -> IO JSCallable
js_pty_on_readable = noJsffi

js_pty_is_readable :: Pty -> IO Bool
js_pty_is_readable = noJsffi

js_pty_on_writable :: Pty -> JSCallable -> IO JSCallable
js_pty_on_writable = noJsffi

js_pty_is_writable :: Pty -> IO Bool
js_pty_is_writable = noJsffi

js_pty_on_signal :: Pty -> JSCallable -> JSCallable -> JSCallable -> JSCallable -> IO JSCallable
js_pty_on_signal = noJsffi


js_call_jsval :: JSCallable -> IO ()
js_call_jsval = noJsffi


js_to_jscallable :: IO () -> IO JSCallable
js_to_jscallable = noJsffi


js_sleep :: Int -> IO ()
js_sleep = noJsffi


js_pty_tcgets :: Pty -> IO JSTermios
js_pty_tcgets = noJsffi

js_pty_tcsets :: Pty -> JSTermios -> IO ()
js_pty_tcsets = noJsffi


js_termios_iflag :: JSTermios -> IO Int
js_termios_iflag = noJsffi

js_termios_oflag :: JSTermios -> IO Int
js_termios_oflag = noJsffi

js_termios_cflag :: JSTermios -> IO Int
js_termios_cflag = noJsffi

js_termios_lflag :: JSTermios -> IO Int
js_termios_lflag = noJsffi

js_termios_cc :: JSTermios -> IO JSByteArray
js_termios_cc = noJsffi

js_to_termios :: Int -> Int -> Int -> Int -> JSByteArray -> IO JSTermios
js_to_termios = noJsffi


js_pty_winsize :: Pty -> IO JSWinsize
js_pty_winsize = noJsffi

js_winsize_col :: JSWinsize -> IO Int
js_winsize_col = noJsffi

js_winsize_row :: JSWinsize -> IO Int
js_winsize_row = noJsffi

#endif