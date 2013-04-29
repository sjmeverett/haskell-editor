
module TextBox where

import Keys
import Display

{-Backwards list-}
data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

{-A cursor is some m thing in the middle with x things round it: things closest
  to the middle are most important, so the first list is backwards-}
type Cursor x m = (Bwd x, m, [x])

{-A dummy placeholder for StringCursors-}
data Here = Here

{-A cursor for a line of text: characters either side and a placeholder in the
  middle-}
type StringCursor = Cursor Char Here

{-A cursor for multiple lines of text: strings (lines) above and below, and a
  StringCursor in the middle-}
type TextCursor = Cursor String StringCursor

{-A data type to represent a TextBox that can be displayed on the screen and typed in-}
data TextBox = TextBox { 
    cursor :: TextCursor,   --the contents of the box, and the position of the cursor
    overwrite :: Bool,      --whether or to overwrite text at the current cursor position
    layout :: (Point, Size) --the position in the text of the top left character in the box, and the size of the box
}


data Damage
  = NoChange       -- nothing at all happened
  | PointChanged   -- moved the cursor but kept the text
  | LineChanged    -- changed text only on the current line
  | LotsChanged    -- changed text off the current line
  deriving (Show, Eq, Ord)


newtype TextBoxAction x = TextBoxAction { runAction :: TextBox -> (x, TextBox) }

instance Monad TextBoxAction where
    return x = TextBoxAction $ \tb -> (x, tb)
    
    (TextBoxAction first) >>= f = TextBoxAction $ \tb -> 
        let (value, tb') = first tb
            (TextBoxAction next) = f value
        in next tb'                     


{-Makes a TextBox from lines of text-}
makeTextBox :: [String] -> Size -> TextBox
makeTextBox lines size = TextBox { cursor = (makeCursor lines), overwrite = False, layout = ((0,0), size) }


{-Handles the given key for the textbox and returns the new viewport, cursor position, and damage report-}
handleKeyAction :: Key -> Size -> TextBoxAction ([String], Point, Damage)
handleKeyAction key size = do
    resized <- resize size
    damage <- handleKey key
    (lines, cursor, scrolled) <- getTextView
    let damage' = if scrolled || resized then LotsChanged else damage
    return (lines, cursor, damage')


{-Resizes the TextBox to the given size, returning true if the size was actually changed-}
resize :: Size -> TextBoxAction Bool
resize size' = TextBoxAction $ \(TextBox { cursor = cur, overwrite = ovw, layout = (scroll, size) })
    -> (size' /= size, TextBox { cursor = cur, overwrite = ovw, layout = (scroll, size') })


{-Makes a TextCursor from lines of text-}
makeCursor :: [String] -> TextCursor
makeCursor [] = (B0, (B0, Here, []), [])
makeCursor (l:ls) = (B0, (B0, Here, l), ls)


{-Ensures that the viewport contains the cursor and then crops that viewport from the text-}
getTextView :: TextBoxAction ([String], Point, Bool)
getTextView = TextBoxAction $ wrapped where
    wrapped (TextBox { cursor = tc@(before, strcur, after), overwrite = ovw, layout = ((sx, sy), size@(w, h)) })
        = ((croppedView, (x - sx', y - sy'), scrollChanged), TextBox { cursor = tc, overwrite = ovw, layout = ((sx', sy'), size) }) 
        where
            croppedView = map (crop sx' w ' ') ((crop sy' h (repeat ' ')) lines)
            crop pos size fill = take size . drop pos . (++(repeat fill))
            (_, x, line) = deactivate strcur
            (_, y, lines) = deactivate (before, Here, line : after)
            
            sx' = intoRange sx x w
            sy' = intoRange sy y h
            scrollChanged = sx' /= sx || sy' /= sy
            
            intoRange val cursor range
                --cursor is inside viewport
                | cursor >= val && cursor < val + range = val
                --cursor is before (above/to the left) viewport
                | cursor < val = cursor
                --cursor is after (below/to the right) viewport
                | otherwise = max 0 (cursor - range)


{-Handles a key being pressed in the TextBox-}
handleKey :: Key -> TextBoxAction Damage

handleKey key = TextBoxAction $ \tb -> wrapped key tb where
    wrapped key (TextBox { cursor = tc, overwrite = ovw, layout = l }) = case key of
        --insert key
        Insert -> (NoChange, (TextBox { cursor = tc, overwrite = not ovw, layout = l }))
        
        --keys that only affect the text cursor
        _ -> (damage, (TextBox { cursor = tc', overwrite = ovw, layout = l })) where
            (damage, tc') = inner ovw key tc
            --special case for when overwrite is true
            inner True (CharKey c) (sz, (cz, Here, _:cs), ss) = (LineChanged, (sz, (cz :< c, Here, cs), ss))
            inner _ key tc = handleTCKey key tc


{-Handle a key which just affects the text cursor-}
handleTCKey :: Key -> TextCursor -> (Damage, TextCursor)
    
--normal typing (or overwrite true at end of line)
handleTCKey (CharKey c) (sz, (cz, Here, cs), ss)
    = (LineChanged, (sz, (cz :< c, Here, cs), ss))
    
--backspace
handleTCKey (Backspace) (sz, (cz :< c, cur, cs), ss)
    = (LineChanged, (sz, (cz, cur, cs), ss))

--backspace at start of line
handleTCKey (Backspace) (sz :< s, (B0, cur, cs), ss)
    = (LotsChanged, (sz, activate (cur, length line, line), ss))
    where line = s ++ cs

--delete
handleTCKey (Delete) (sz, (cz, cur, c : cs), ss)
    = (LineChanged, (sz, (cz, cur, cs), ss))

--delete at end of line
handleTCKey (Delete) (sz, (cz, cur, []), s:ss)
    = (LotsChanged, (sz, (cz, cur, s), ss))

--return
handleTCKey (Return) (sz, (cz, cur, cs), ss)
    = (LotsChanged, (sz :< line, (activate (cur', 0, cs)), ss))
    where (cur', _, line) = deactivate (cz, cur, [])  

--left arrow
handleTCKey (ArrowKey Normal LeftArrow) (sz, (cz :< c, cur, cs), ss)
    = (PointChanged, (sz, (cz, cur, c:cs), ss))

--left arrow at start of line                
handleTCKey (ArrowKey Normal LeftArrow) (sz :< s, c@(B0, cur, cs), ss)
    = (PointChanged, (sz, (activate (cur', length s, s)), line : ss))
    where (cur', _, line) = deactivate c

--right arrow
handleTCKey (ArrowKey Normal RightArrow) (sz, (cz, cur, c:cs), ss)
    = (PointChanged, (sz, (cz :< c, cur, cs), ss))

--right arrow at end of line
handleTCKey (ArrowKey Normal RightArrow) (sz, c@(cz, cur, []), s:ss)
    = (PointChanged, (sz :< line, (activate (cur', 0, s)), ss))
    where (cur', _, line) = deactivate c
    
--up arrow
handleTCKey (ArrowKey Normal UpArrow) (sz :< s, c, ss)
    = (PointChanged, (sz, activate (cur', n, s), line : ss))
    where (cur', n, line) = deactivate c
    
--down arrow
handleTCKey (ArrowKey Normal DownArrow) (sz, c, s : ss)
    = (PointChanged, (sz :< line, activate (cur', n, s), ss))
    where (cur', n, line) = deactivate c
    
--home key
handleTCKey (Home) (sz, (cz, cur, cs), ss)
    = (PointChanged, (sz, cursor, ss))
    where cursor = (B0, cur, (makeForward cz cs))
    
--end key
handleTCKey (End) (sz, (cz, cur, cs), ss)
    = (PointChanged, (sz, cursor, ss))
    where cursor = ((makeBackward cz cs), cur, [])

--default
handleTCKey _ tc = (NoChange, tc)


        
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


makeForward :: Bwd x -> [x] -> [x]
makeForward B0 bs = bs
makeForward (as :< a) bs
    = (makeForward as (a:bs))
    

makeBackward :: Bwd x -> [x] -> Bwd x
makeBackward as [] = as
makeBackward as (b:bs)
    = (makeBackward (as :< b) bs)
