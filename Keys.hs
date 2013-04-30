{-The Keys module defines the getKey function and some associated datatypes,
  to allow keyboard input with ANSI escape sequences to be parsed for easy
  pattern matching.-}

module Keys (getKey, ArrowDir(..), Modifier(..), Key(..)) where

import System.IO(hReady,stdin)
import Data.Char(chr)

{-For use with the Key type-}
data ArrowDir = UpArrow | DownArrow | LeftArrow | RightArrow deriving Show
data Modifier = Normal | Shift | Control | Alt deriving Show

{-A parsed key-}
data Key
  = CharKey Char
  | ArrowKey Modifier ArrowDir
  | ControlKey Modifier Char
  | Return
  | Backspace
  | Delete
  | Escape
  | Home
  | End
  | Insert
  | FunctionKey Int
  deriving Show


{-Holds a mapping of escape sequences etc to keys-}
type KeyMap = [(String, Key)]


{-Filters the map to contain only items starting with the specified
  character.-}
filterKeyMap :: Char -> KeyMap -> KeyMap
filterKeyMap c km = [(xs, k) | (x:xs, k) <- km, x == c]


{-Reads one or more characters from stdin and returns the associated
  Key value, using the supplied KeyMap to parse escape sequences etc-}
getKey :: IO (Maybe Key)
getKey = do
    c <- getChar
    b <- hReady stdin
    
    if c == '\ESC' && not b then
        --escape on its own
        return (Just Escape)
    else if c >= ' ' && c < '\DEL' then
        --normal typed key
        return (Just (CharKey c))
    else
        --any other key
        specialKey c standardKeyMap
    where
        specialKey c km = lookupKey (filterKeyMap c km)
        lookupKey [] = return Nothing
        lookupKey km = case lookup "" km of
            Just k -> return (Just k)
            _ -> do
                c <- getChar
                specialKey c km


{-Escape code arrow directions for key map-}
directions :: [(Char, ArrowDir)]
directions = [('A', UpArrow), ('B', DownArrow),
              ('C', RightArrow), ('D', LeftArrow)]


{-A mapping of special keys to Key values-}
standardKeyMap :: KeyMap
standardKeyMap =
    [("\n", Return)] ++
    [("\r", Return)] ++
    [("\b", Backspace)] ++
    [("\DEL", Backspace)] ++
    [([chr c], ControlKey Normal (chr (c + 0x40))) | c <- [1..26]] ++
    [("\ESC" ++ [chr c], ControlKey Alt (chr (c + 0x40))) | c <- [1..26]] ++
    [("\ESC[" ++ [c], ArrowKey Normal d) | (c, d) <- directions] ++
    [("\ESC[1;2" ++ [c], ArrowKey Shift d) | (c, d) <- directions] ++
    [("\ESC[1;5" ++ [c], ArrowKey Control d) | (c, d) <- directions] ++
    [("\ESC[3~", Delete)] ++
    [("\ESCOH", Home)] ++
    [("\ESC[1~", Home)] ++
    [("\ESCOF", End)] ++
    [("\ESC[4~", End)] ++
    [("\ESC[2~", Insert)] ++
    [("\ESCOQ", FunctionKey 2)] ++
    [("\ESCOR", FunctionKey 3)] ++
    [("\ESCOS", FunctionKey 4)] ++
    [("\ESC[15~", FunctionKey 5)] ++
    [("\ESC[17~", FunctionKey 6)] ++
    [("\ESC[18~", FunctionKey 7)] ++
    [("\ESC[19~", FunctionKey 8)] ++
    [("\ESC[20~", FunctionKey 9)] ++
    [("\ESC[24~", FunctionKey 12)]

