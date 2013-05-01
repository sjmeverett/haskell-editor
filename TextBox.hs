
module TextBox where

import Keys
import Display
import Control.Monad

{-Backwards list-}
data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

{-A cursor is some m thing in the middle with x things round it: things closest
  to the middle are most important, so the first list is backwards-}
type Cursor x m = (Bwd x, m, [x])

{-A dummy placeholder for StringCursors-}
data Here = Here deriving Show

{-A cursor for a line of text: characters either side and a placeholder in the
  middle-}
type StringCursor = Cursor Char Here

{-A cursor for multiple lines of text: strings (lines) above and below, and a
  StringCursor in the middle-}
type TextCursor = Cursor String StringCursor

{-A data type to represent a TextBox that can be displayed on the screen and typed in-}
data TextBox = TextBox { 
    --the contents of the box, and the position of the cursor
    cursor :: TextCursor,
    --whether or to overwrite text at the current cursor position
    overwrite :: Bool,
    --the position of the box on screen, the position in the text of the top
    --left character in the box, and the size of the box
    layout :: (Point, Point, Size) 
} deriving Show


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
makeTextBox :: [String] -> TextBox
makeTextBox lines = TextBox { cursor = (makeCursor lines), overwrite = False, layout = ((0,0), (0,0), (0,0)) }


{-Makes a TextCursor from lines of text-}
makeCursor :: [String] -> TextCursor
makeCursor [] = (B0, (B0, Here, []), [])
makeCursor (l:ls) = (B0, (B0, Here, l), ls)


{-Gets the text contained in a textbox-}
getText :: TextBox -> String
getText tb = (unlines . getLines) tb


{-Gets the lines contained in a textbox-}
getLines :: TextBox -> [String]
getLines (TextBox { cursor = (before, strcur, after), overwrite = _, layout = _ }) =
    let (_, _, line) = deactivate strcur
        (_, _, lines) = deactivate (before, Here, line : after)
    
    in lines
    
    
{-Deletes all the text in a textbox-}
clear :: TextBox -> TextBox
clear (TextBox { cursor = _, overwrite = ovw, layout = l})
    = TextBox { cursor = makeCursor [], overwrite = ovw, layout = l}


{-Handles the given key for the given TextBox and updates the screen correspondingly-}
updateTextBox :: Key -> TextBox -> IO TextBox
updateTextBox key tb = do
    let (damage, tb') = runAction (inner key) tb
    paint damage tb'
    return tb'
    
    where 
        inner key = do
            damage <- handleKey key
            scrolled <- scrollToCursor
            return $ if scrolled then LotsChanged else damage


{-Reloacates the TextBox to the given location and size, returning true if anything was actually changed-}
relocate :: Point -> Size -> TextBoxAction Bool
relocate pos' size' = TextBoxAction $ \(TextBox { cursor = cur, overwrite = ovw, layout = (pos, scroll, size) })
    -> (pos' /= pos || size' /= size, TextBox { cursor = cur, overwrite = ovw, layout = (pos', scroll, size') })


{-Ensures that the viewport contains the cursor-}
scrollToCursor :: TextBoxAction Bool
scrollToCursor = TextBoxAction $ \(TextBox { cursor = tc@(before, strcur, after), overwrite = ovw, layout = (pos, (sx, sy), size@(w, h)) }) ->
    let (_, x, line) = deactivate strcur
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
            | otherwise = max 0 (cursor - range `div` 2)
    
    in (scrollChanged, TextBox { cursor = tc, overwrite = ovw, layout = (pos, (sx', sy'), size) })


{-Appends the given lines to the textbox and moves the cursor to the first
  character on the last line-}
appendLines :: [String] -> TextBoxAction ()
appendLines newlines = TextBoxAction $ \(TextBox { cursor = (before, strcur, after), overwrite = ovw, layout = lay }) ->
    let (_, _, line) = deactivate strcur
        tc' = (makeBackward before (line : after ++ init newlines), activate (Here, 0, last newlines), [])
    
    in ((), TextBox { cursor = tc', overwrite = ovw, layout = lay })
            
        

{-Appends the given lines to the textbox, scrolls it to the last line and
  repaints it on screen-}
appendAndScroll :: [String] -> TextBox -> IO (TextBox)
appendAndScroll lines tb = do
    let action = do
            appendLines lines
            scrollToCursor
            
        (_, tb') = runAction action tb
        
    paint LotsChanged tb'
    return tb'
    
    
{-Sets the text in a textbox, puts the cursor to the end, and paints it on screen-}
setLines :: [String] -> TextBox -> IO (TextBox)
setLines lines (TextBox { cursor = (before, strcur, after), overwrite = ovw, layout = lay}) = do
    let tb = TextBox { cursor = (makeBackward B0 (init lines), (makeBackward B0 (last lines), Here, []), []), overwrite = ovw, layout = lay }
    paint LotsChanged tb
    return tb
    


{-Crops the viewport from the text-}
getTextView :: TextBox -> ([String], Point)
getTextView tb@(TextBox { cursor = tc@(before, strcur, after), overwrite = ovw, layout = lay@((px, py), (sx, sy), (w, h)) }) =
    let croppedView = map (crop sx w ' ') ((crop sy h (repeat ' ')) lines)
        crop pos size fill = take size . drop pos . (++(repeat fill))
        (_, x, line) = deactivate strcur
        (_, y, lines) = deactivate (before, Here, line : after)
           
    in (croppedView, (x - sx + px, y - sy + py)) 
            


{-Paints the given textbox on screen-}
paint :: Damage -> TextBox -> IO ()
paint damage tb@(TextBox { cursor = _, overwrite = _, layout = (pos@(px, py), _, (tw, th)) }) = do
    (sw, sh) <- screenSize
    let (view, cursor@(cx, cy)) = getTextView tb
    
    case damage of
        LotsChanged -> do
            if (tw /= sw) then do
                foldM_ putln pos view
            else do
                cursorToPoint pos
                mapM_ putStr view
        
        LineChanged -> do
            cursorToPoint (px, cy)
            putStr (view !! (cy - py))
            
        _ -> return ()
        
    cursorToPoint cursor
        
    where
        putln pos@(x,y) line = do
            cursorToPoint pos
            putStr line
            return (x, y + 1)



{-Handles a key being pressed in the TextBox-}
handleKey :: Key -> TextBoxAction Damage

handleKey key = TextBoxAction $ \tb@(TextBox { cursor = tc, overwrite = ovw, layout = l }) ->
    case key of
        --insert key
        Insert -> (NoChange, (TextBox { cursor = tc, overwrite = not ovw, layout = l }))
        
        --keys that only affect the text cursor
        _ -> (damage, (TextBox { cursor = tc', overwrite = ovw, layout = l }))
            where
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
    = (LotsChanged, (sz, (makeBackward B0 s, cur, cs), ss))

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
