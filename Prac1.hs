{--------------------------------------------------------------}
{- CS410 Advanced Functional Programming                      -}
{- Practical 1: A Text Editor                                 -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- SUBMIT YOUR VERSION OF THIS FILE BY EMAIL TO CONOR         -}
{- DEADLINE: midnight, Thursday 11 October                    -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- IDENTIFY YOURSELF:                                         -}
{- Name: Stewart MacKenzie-Leigh                              -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- THIS CODE WILL COMPILE UNDER UNIX BUT NOT WINDOWS          -}
{-                                                            -}
{- To compile the project, use shell command                  -}
{-   make                                                     -}
{- To run your editor, try                                    -}
{-   ./credit <filename>                                      -}
{- or                                                         -}
{-   ./credit                                                 -}
{- to start from blank.                                       -}
{- To quit the editor, press ESCAPE.                          -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- This practical makes use of a bunch of other files I've    -}
{- written, including the layout file from last time. This is -}
{- the only file you should modify. Don't rename this file!   -} 
{--------------------------------------------------------------}

module Prac1 where

import Block
import Overlay

{--------------------------------------------------------------}
{- This module starts with some equipment I've provided for   -}
{- you. To get going, you should not need to make any changes -}
{- but to try more sophisticated things, e.g. editing with    -}
{- selection, cut, and paste, you may want to make changes.   -}
{-                                                            -}
{- Your main mission is to implement handleKey, down below.   -}
{--------------------------------------------------------------}

{- From the lecture, here's that type of backward lists. -}

data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

{- A slight improvement on the lecture: this is a cursor with x things
   round the outside, and an m in the middle. The idea is that we keep
   everything in exactly the right order, so you can always see what's
   where. -}

type Cursor x m = (Bwd x, m, [x])

{- Here's something to put in the middle, to show where you are. -}

data Here = Here0 | Here Bool String deriving Show

{- If you start working with selections, you may wish to modify the
   Here type to account for the current state, e.g. {no selection,
   selection left of cursor, or selection right of cursor}.      -}

{- For one line, we have characters either side, and Here in the middle. -}

type StringCursor = Cursor Char Here

{- For multiple lines, we have strings above and below, and a StringCursor
   in the middle. -}

type TextCursor = Cursor String StringCursor



{- Useful equipment: deactivate turns a cursor into a list by shuffling
   everything to the right, but it also tells you /numerically/ where the
   cursor was. This might help you implement up and down, for example. -}

deactivate :: Cursor x Here -> (Here, Int, [x])
deactivate c = outward 0 c where
  outward i (B0, cur, xs)       = (cur, i, xs)
  outward i (xz :< x, cur, xs)  = outward (i + 1) (xz, cur, x : xs)

{- Activate turns a list into a cursor open at the given position, or as
   near as it gets. -}

activate :: (Here, Int, [x]) -> Cursor x Here
activate (cur, i, xs) = inward i (B0, cur, xs) where
  inward _ c = c  -- we can go no further
  inward 0 c = c  -- we should go no further
  inward i (xz, cur', x : xs)  = inward (i - 1) (xz :< x, cur', xs)  -- and on!

{- Now, if you give me a TextCursor, I can compute the corresponding
   Layout Box, together with the coordinates of Here.
   This is how my code figures out what to display and where to put the
   cursor. -}

whatAndWhere :: TextCursor -> (Layout Box, Point)
whatAndWhere (czz, cur, css) = (foldr (joinV . layS) layZ strs, (x, y)) where
  (tc, x, cs) = deactivate cur
  (_, y, strs) = deactivate (czz, Here0, cs : css)

{- Next, you'll need some model of keystrokes. Here's a type describing
   some keystrokes. You may want more. -}

data ArrowDir = UpArrow | DownArrow | LeftArrow | RightArrow
data Modifier = Normal | Shift | Control

data Key
  = CharKey Char                -- an ordinary printable character
  | ArrowKey Modifier ArrowDir  -- an arrow key
  | Return
  | Backspace
  | Delete
  | Quit
  | Home
  | End
  | Insert
  | CtrlK
  | CtrlU

{- Keys come in as standard ANSI escape sequences. You can look 'em up
   online. Feel free to extend escapeKeys so that more keystrokes get
   translated. -}

directions :: [(Char, ArrowDir)]
directions = [('A', UpArrow), ('B', DownArrow),
              ('C', RightArrow), ('D', LeftArrow)]

escapeKeys :: [(String, Key)]
escapeKeys =
  [([c], ArrowKey Normal d) | (c, d) <- directions] ++
  [("1;2" ++ [c], ArrowKey Shift d) | (c, d) <- directions] ++
  [("1;5" ++ [c], ArrowKey Control d) | (c, d) <- directions] ++
  [("3~", Delete)] ++
  [("H", Home)] ++
  [("F", End)] ++
  [("1~", Home)] ++
  [("4~", End)] ++
  [("2~", Insert)]
  

{- Last but not least, you get to tell my code how much damage you've done.
   This makes the redrawing more efficient: if you've done less damage to
   the file, my code needs to do less to update. If in doubt, overestimate
   the damage: a slow display is better than a broken display. -}

data Damage
  = NoChange       -- use this if nothing at all happened
  | PointChanged   -- use this if you moved the cursor but kept the text
  | LineChanged    -- use this if you changed text only on the current line
  | LotsChanged    -- use this if you changed text off the current line
  deriving (Show, Eq, Ord)

{--------------------------------------------------------------------------}
{- AT LAST, YOUR BIT!                                                     -}
{- Given a Key and an initial TextCursor, either reject the keystroke or  -}
{- return a modified cursor, with an overestimate of the damage you've    -}
{- you've done. To give you the idea, I've supplied a broken version of   -}
{- ordinary typing, which you get to fix.                                 -}
{-                                                                        -}
{- Be creative!                                                           -}
{--------------------------------------------------------------------------}

handleKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)

--NORMAL CHARACTERS
handleKey (CharKey c) (sz, (cz, Here False clip, cs), ss)
    = Just (LineChanged, (sz, (cz :< c, Here False clip, cs), ss))
    
handleKey (CharKey c) (sz, (cz, Here True clip, _ : cs), ss)
    = Just (LineChanged, (sz, (cz :< c, Here True clip, cs), ss))
    
handleKey (CharKey c) (sz, (cz, Here True clip, []), ss)
    = Just (LineChanged, (sz, (cz :< c, Here True clip, []), ss))    

--SPECIAL KEYS      
--backspace
handleKey (Backspace) (sz, (cz :< c, cur, cs), ss)
    = Just (LineChanged, (sz, (cz, cur, cs), ss))

handleKey (Backspace) (sz :< s, (B0, cur, cs), ss)
    = Just (LotsChanged, (sz, activate (cur, length s, line), ss))
    where line = s ++ cs


--delete
handleKey (Delete) (sz, (cz, cur, c : cs), ss)
    = Just (LineChanged, (sz, (cz, cur, cs), ss))

handleKey (Delete) (sz, (cz, cur, []), s:ss)
    = Just (LotsChanged, (sz, (cz, cur, s), ss))

--return
handleKey (Return) (sz, (cz, cur, cs), ss)
    = Just (LotsChanged, (sz :< line, (activate (cur', 0, cs)), ss))
    where (cur', _, line) = deactivate (cz, cur, [])  


--ARROWS
--left arrow
handleKey (ArrowKey Normal LeftArrow) (sz, (cz :< c, cur, cs), ss)
    = Just (PointChanged, (sz, (cz, cur, c:cs), ss))
                
handleKey (ArrowKey Normal LeftArrow) (sz :< s, c@(B0, cur, cs), ss)
    = Just (PointChanged, (sz, (activate (cur', length s, s)), line : ss))
    where (cur', _, line) = deactivate c

--right arrow
handleKey (ArrowKey Normal RightArrow) (sz, (cz, cur, c:cs), ss)
    = Just (PointChanged, (sz, (cz :< c, cur, cs), ss))

handleKey (ArrowKey Normal RightArrow) (sz, c@(cz, cur, []), s:ss)
    = Just (PointChanged, (sz :< line, (activate (cur', 0, s)), ss))
    where (cur', _, line) = deactivate c
    
--up arrow
handleKey (ArrowKey Normal UpArrow) (sz :< s, c, ss)
    = Just (PointChanged, (sz, activate (cur', n, s), line : ss))
    where (cur', n, line) = deactivate c
    
--down arrow
handleKey (ArrowKey Normal DownArrow) (sz, c, s : ss)
    = Just (PointChanged, (sz :< line, activate (cur', n, s), ss))
    where (cur', n, line) = deactivate c
    
--OTHER STUFF
--home key
handleKey (Home) (sz, (cz, cur, cs), ss)
    = Just (PointChanged, (sz, cursor, ss))
    where cursor = (B0, cur, (makeForward cz cs))
    
--end key
handleKey (End) (sz, (cz, cur, cs), ss)
    = Just (PointChanged, (sz, cursor, ss))
    where cursor = ((makeBackward cz cs), cur, [])

--insert
handleKey (Insert) (sz, (cz, Here False clip, cs), ss)
    = Just (NoChange, (sz, (cz, Here True clip, cs), ss))

handleKey (Insert) (sz, (cz, Here True clip, cs), ss)
    = Just (NoChange, (sz, (cz, Here False clip, cs), ss))

--ctrl-k
handleKey (CtrlK) (sz, c, s : ss)
    = Just (LotsChanged, (sz, cursor, ss)) where
        (Here ins clip, n, line) = deactivate c
        cursor = activate ((Here ins line), 0, s)
        
handleKey (CtrlK) (sz, c, [])
    = Just (LotsChanged, (sz, cursor, [])) where
        (Here ins clip, n, line) = deactivate c
        cursor = activate ((Here ins line), 0, [])

--ctrl-u
handleKey (CtrlU) (sz, (cz, c@(Here ins clip), cs), ss)
    = Just (LotsChanged, (sz :< line, activate (Here ins clip, 0, cs), ss))
    where line = makeForward cz clip

--DEFAULT CASE

handleKey _ _ = Nothing


--Quit
handleQuit :: TextCursor -> Bool
handleQuit tc = True

--SOME FUNCTIONS
makeForward :: Bwd x -> [x] -> [x]
makeForward B0 bs = bs
makeForward (as :< a) bs
    = (makeForward as (a:bs))
    

makeBackward :: Bwd x -> [x] -> Bwd x
makeBackward as [] = as
makeBackward as (b:bs)
    = (makeBackward (as :< b) bs)
