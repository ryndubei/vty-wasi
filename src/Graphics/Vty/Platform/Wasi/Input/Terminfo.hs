-- | Terminfo-oriented terminal input parser.
--
-- This module is exposed for testing purposes only; applications should
-- never need to import this directly.
module Graphics.Vty.Platform.Wasi.Input.Terminfo
  ( classifyMapForTerm
  , specialSupportKeys
  , capsClassifyMap
  , keysFromCapsTable
  , universalTable
  , visibleChars
  )
where

import Data.Maybe (mapMaybe)
import Graphics.Vty.Input.Events
import qualified Graphics.Vty.Platform.Wasi.Input.Terminfo.ANSIVT as ANSIVT

import Control.Arrow
import System.Terminfo
import System.Terminfo.Caps

-- | Queries the terminal for all capability-based input sequences and
-- then adds on a terminal-dependent input sequence mapping.
--
-- For reference see:
--
-- * http://vimdoc.sourceforge.net/htmldoc/term.html
--
-- * vim74/src/term.c
--
-- * http://invisible-island.net/vttest/
--
-- * http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
--
-- Terminfo is incomplete. The vim source implies that terminfo is also
-- incorrect. Vty assumes that the internal terminfo table added to the
-- system-provided terminfo table is correct.
--
-- The procedure used here is:
--
-- 1. Build terminfo table for all caps. Missing caps are not added.
--
-- 2. Add tables for visible chars, esc, del, ctrl, and meta.
--
-- 3. Add internally-defined table for given terminal type.
--
-- Precedence is currently implicit in the 'compile' algorithm.
classifyMapForTerm :: String -> TIDatabase -> ClassifyMap
classifyMapForTerm termName term =
    concat $ capsClassifyMap term keysFromCapsTable
           : universalTable
           : termSpecificTables termName

-- | The key table applicable to all terminals.
--
-- Note that some of these entries are probably only applicable to
-- ANSI/VT100 terminals.
universalTable :: ClassifyMap
universalTable = concat [visibleChars, ctrlChars, ctrlMetaChars, specialSupportKeys]

capsClassifyMap :: TIDatabase -> [(StrTermCap,Event)] -> ClassifyMap
capsClassifyMap terminal table = [(x,y) | (Just x,y) <- map extractCap table]
    where
      extractCap = first (queryStrTermCap terminal)

-- | Tables specific to a given terminal that are not derivable from
-- terminfo.
--
-- Note that this adds the ANSI/VT100/VT50 tables regardless of term
-- identifier.
termSpecificTables :: String -> [ClassifyMap]
termSpecificTables _termName = ANSIVT.classifyTable

-- | Visible characters in the ISO-8859-1 and UTF-8 common set.
--
-- We limit to < 0xC1. The UTF8 sequence detector will catch all values
-- 0xC2 and above before this classify table is reached.
visibleChars :: ClassifyMap
visibleChars = [ ([x], EvKey (KChar x) [])
               | x <- [' ' .. toEnum 0xC1]
               ]

-- | Non-printable characters in the ISO-8859-1 and UTF-8 common set
-- translated to ctrl + char.
--
-- This treats CTRL-i the same as tab.
ctrlChars :: ClassifyMap
ctrlChars =
    [ ([toEnum x],EvKey (KChar y) [MCtrl])
    | (x,y) <- zip [0..31] ('@':['a'..'z']++['['..'_'])
    , y /= 'i'  -- Resolve issue #3 where CTRL-i hides TAB.
    , y /= 'h'  -- CTRL-h should not hide BS
    ]

-- | Ctrl+Meta+Char
ctrlMetaChars :: ClassifyMap
ctrlMetaChars = mapMaybe f ctrlChars
    where
        f (s, EvKey c m) = Just ('\ESC':s, EvKey c (MMeta:m))
        f _ = Nothing

-- | Esc, meta-esc, delete, meta-delete, enter, meta-enter.
specialSupportKeys :: ClassifyMap
specialSupportKeys =
    [ ("\ESC\ESC[5~",EvKey KPageUp [MMeta])
    , ("\ESC\ESC[6~",EvKey KPageDown [MMeta])
    -- special support for ESC
    , ("\ESC",EvKey KEsc []), ("\ESC\ESC",EvKey KEsc [MMeta])
    -- Special support for backspace
    , ("\DEL",EvKey KBS []), ("\ESC\DEL",EvKey KBS [MMeta]), ("\b",EvKey KBS [])
    -- Special support for Enter
    , ("\ESC\^J",EvKey KEnter [MMeta]), ("\^J",EvKey KEnter [])
    -- explicit support for tab
    , ("\t", EvKey (KChar '\t') [])
    ]

-- | A classification table directly generated from terminfo cap
-- strings.  These are:
--
-- * ka1 - keypad up-left
--
-- * ka3 - keypad up-right
--
-- * kb2 - keypad center
--
-- * kbs - keypad backspace
--
-- * kbeg - begin
--
-- * kcbt - back tab
--
-- * kc1 - keypad left-down
--
-- * kc3 - keypad right-down
--
-- * kdch1 - delete
--
-- * kcud1 - down
--
-- * kend - end
--
-- * kent - enter
--
-- * kf0 - kf63 - function keys
--
-- * khome - KHome
--
-- * kich1 - insert
--
-- * kcub1 - left
--
-- * knp - next page (page down)
--
-- * kpp - previous page (page up)
--
-- * kcuf1 - right
--
-- * kDC - shift delete
--
-- * kEND - shift end
--
-- * kHOM - shift home
--
-- * kIC - shift insert
--
-- * kLFT - shift left
--
-- * kRIT - shift right
--
-- * kcuu1 - up
keysFromCapsTable :: [(StrTermCap, Event)]
keysFromCapsTable =
    [ (KeyA1,   EvKey KUpLeft    [])
    , (KeyA3,   EvKey KUpRight   [])
    , (KeyB2,   EvKey KCenter    [])
    , (KeyBackspace,   EvKey KBS        [])
    , (KeyBeg,  EvKey KBegin     [])
    , (KeyBtab,  EvKey KBackTab   [])
    , (KeyC1,   EvKey KDownLeft  [])
    , (KeyC3,   EvKey KDownRight [])
    , (KeyDc, EvKey KDel       [])
    , (KeyDown, EvKey KDown      [])
    , (KeyEnd,  EvKey KEnd       [])
    , (KeyEnter,  EvKey KEnter     [])
    , (KeyHome, EvKey KHome      [])
    , (KeyIc, EvKey KIns       [])
    , (KeyLeft, EvKey KLeft      [])
    , (KeyNpage,   EvKey KPageDown  [])
    , (KeyPpage,   EvKey KPageUp    [])
    , (KeyRight, EvKey KRight     [])
    , (KeySdc,   EvKey KDel       [MShift])
    , (KeySend,  EvKey KEnd       [MShift])
    , (KeyShome,  EvKey KHome      [MShift])
    , (KeySic,   EvKey KIns       [MShift])
    , (KeySleft,  EvKey KLeft      [MShift])
    , (KeySright,  EvKey KRight     [MShift])
    , (KeyUp, EvKey KUp        [])
    , (KeyF0, EvKey (KFun 0) [])
    , (KeyF1, EvKey (KFun 1) [])
    , (KeyF2, EvKey (KFun 2) [])
    , (KeyF3, EvKey (KFun 3) [])
    , (KeyF4, EvKey (KFun 4) [])
    , (KeyF5, EvKey (KFun 5) [])
    , (KeyF6, EvKey (KFun 6) [])
    , (KeyF7, EvKey (KFun 7) [])
    , (KeyF8, EvKey (KFun 8) [])
    , (KeyF9, EvKey (KFun 9) [])
    , (KeyF10, EvKey (KFun 10) [])
    , (KeyF11, EvKey (KFun 11) [])
    , (KeyF12, EvKey (KFun 12) [])
    , (KeyF13, EvKey (KFun 13) [])
    , (KeyF14, EvKey (KFun 14) [])
    , (KeyF15, EvKey (KFun 15) [])
    , (KeyF16, EvKey (KFun 16) [])
    , (KeyF17, EvKey (KFun 17) [])
    , (KeyF18, EvKey (KFun 18) [])
    , (KeyF19, EvKey (KFun 19) [])
    , (KeyF20, EvKey (KFun 20) [])
    , (KeyF21, EvKey (KFun 21) [])
    , (KeyF22, EvKey (KFun 22) [])
    , (KeyF23, EvKey (KFun 23) [])
    , (KeyF24, EvKey (KFun 24) [])
    , (KeyF25, EvKey (KFun 25) [])
    , (KeyF26, EvKey (KFun 26) [])
    , (KeyF27, EvKey (KFun 27) [])
    , (KeyF28, EvKey (KFun 28) [])
    , (KeyF29, EvKey (KFun 29) [])
    , (KeyF30, EvKey (KFun 30) [])
    , (KeyF31, EvKey (KFun 31) [])
    , (KeyF32, EvKey (KFun 32) [])
    , (KeyF33, EvKey (KFun 33) [])
    , (KeyF34, EvKey (KFun 34) [])
    , (KeyF35, EvKey (KFun 35) [])
    , (KeyF36, EvKey (KFun 36) [])
    , (KeyF37, EvKey (KFun 37) [])
    , (KeyF38, EvKey (KFun 38) [])
    , (KeyF39, EvKey (KFun 39) [])
    , (KeyF40, EvKey (KFun 40) [])
    , (KeyF41, EvKey (KFun 41) [])
    , (KeyF42, EvKey (KFun 42) [])
    , (KeyF43, EvKey (KFun 43) [])
    , (KeyF44, EvKey (KFun 44) [])
    , (KeyF45, EvKey (KFun 45) [])
    , (KeyF46, EvKey (KFun 46) [])
    , (KeyF47, EvKey (KFun 47) [])
    , (KeyF48, EvKey (KFun 48) [])
    , (KeyF49, EvKey (KFun 49) [])
    , (KeyF50, EvKey (KFun 50) [])
    , (KeyF51, EvKey (KFun 51) [])
    , (KeyF52, EvKey (KFun 52) [])
    , (KeyF53, EvKey (KFun 53) [])
    , (KeyF54, EvKey (KFun 54) [])
    , (KeyF55, EvKey (KFun 55) [])
    , (KeyF56, EvKey (KFun 56) [])
    , (KeyF57, EvKey (KFun 57) [])
    , (KeyF58, EvKey (KFun 58) [])
    , (KeyF59, EvKey (KFun 59) [])
    , (KeyF60, EvKey (KFun 60) [])
    , (KeyF61, EvKey (KFun 61) [])
    , (KeyF62, EvKey (KFun 62) [])
    , (KeyF63, EvKey (KFun 63) [])
    ]
