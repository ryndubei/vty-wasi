{-# LANGUAGE CPP #-}
#if defined(wasi_HOST_OS)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Vty.Platform.Wasi.Pty.JSFFI
  ( JSByteArray(..)
  , JSTermios(..)
  , JSWinsize(..)
  , JSCallable(..)
  , JSFd(..)
  , js_byte_array_length
  , js_subarray
  , js_memory
  , js_copy_to_byte_array
  , js_typeof
  , js_call_jsval
  , js_to_jscallable
  , js_termios_iflag
  , js_termios_oflag
  , js_termios_cflag
  , js_termios_lflag
  , js_termios_cc
  , js_to_termios
  , js_winsize_col
  , js_winsize_row
  , js_get_fd
  , js_fd_ioctl_tcgets
  , js_fd_ioctl_tcsets
  , js_fd_ioctl_tiocgwinsz
  , js_fd_on_sigwinch

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

newtype JSTermios = JSTermios JSVal

newtype JSWinsize = JSWinsize JSVal

newtype JSCallable = JSCallable JSVal

newtype JSFd = JSFd JSVal

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


foreign import javascript unsafe "typeof $1"
  js_typeof :: JSVal -> IO JSString


foreign import javascript unsafe "dynamic"
  js_call_jsval :: JSCallable -> IO ()

foreign import javascript "wrapper"
  js_to_jscallable :: IO () -> IO JSCallable


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


foreign import javascript unsafe "$1[0]"
  js_winsize_col :: JSWinsize -> IO Int

foreign import javascript unsafe "$1[1]"
  js_winsize_row :: JSWinsize -> IO Int

{-
HACK

Assumption: @bjorn3/browser_wasi_shim is the wasi implementation.
(or #wasi.fds is otherwise an array of file descriptor objects)
This is the default when node is not being used.

While WASI does not define an ioctl operation, there is nothing stopping
us from just defining an ioctl method on certain file descriptors
that we care about. 
-}

foreign import javascript unsafe "__ghc_wasm_jsffi_dyld.#wasi.fds[$1]"
  js_get_fd :: Int -> IO JSFd

foreign import javascript unsafe "$1?.ioctl('TCGETS')"
  js_fd_ioctl_tcgets :: JSFd -> IO JSTermios

foreign import javascript unsafe "$1?.ioctl('TCSETS', $2)"
  js_fd_ioctl_tcsets :: JSFd -> JSTermios -> IO ()

foreign import javascript unsafe "$1?.ioctl('TIOCGWINSZ')"
  js_fd_ioctl_tiocgwinsz :: JSFd -> IO JSWinsize

{-
HACK (2)

Similarly, there is nothing stopping us from defining onSignal methods for
"signals" related to that file descriptor.
-}

foreign import javascript unsafe "$1?.onSignal(sig => {if (sig === 'SIGWINCH') { return $2() }}).dispose"
  js_fd_on_sigwinch :: JSFd -> JSCallable -> IO JSCallable

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


js_typeof :: JSVal -> IO JSString
js_typeof = noJsffi


js_call_jsval :: JSCallable -> IO ()
js_call_jsval = noJsffi


js_to_jscallable :: IO () -> IO JSCallable
js_to_jscallable = noJsffi


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


js_winsize_col :: JSWinsize -> IO Int
js_winsize_col = noJsffi

js_winsize_row :: JSWinsize -> IO Int
js_winsize_row = noJsffi

js_get_fd :: Int -> IO JSVal
js_get_fd = noJsffi

js_fd_ioctl_tcgets :: JSFd -> IO JSVal
js_fd_ioctl_tcgets = noJsffi

js_fd_ioctl_tcsets :: JSFd -> JSTermios -> IO ()
js_fd_ioctl_tcsets = noJsffi

js_fd_ioctl_tiocgwinsz :: JSFd -> IO JSWinsize
js_fd_ioctl_tiocgwinsz = noJsffi

js_fd_on_sigwinch :: JSFd -> JSCallable -> IO JSVal
js_fd_on_sigwinch = noJsffi

#endif