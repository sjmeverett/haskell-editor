
module Main where

import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.Environment
import FoulParser
import FOUL

type HistoryCursor = Cursor String Here

{-A TextEditor has a file edit box, a console output, a console input, a command history, a compiled program, and a size-}
type TextEditor = (TextBox, TextBox, TextBox, HistoryCursor, Prog, Size)


dolayout :: TextEditor -> IO TextEditor
dolayout te@(fb, co, ci, ch, pr, sz@(w, h)) = do
    refresh
    sz'@(w', h') <- screenSize
    
    let (_, fb') = runAction (relocate (0, 0) (w', fbheight)) fb
        (_, co') = runAction (relocate (0, fbheight + 1) (w', coheight)) co
        (_, ci') = runAction (relocate (0, fbheight + coheight + 1)(w', 1)) ci
        fbheight = h' - coheight - 2
        coheight = 8
    
    if sz' /= sz then do
        paint LotsChanged fb'
        paint LotsChanged co'
        paint LotsChanged ci'
        cursorToPoint (0, fbheight)
        putStr (yellow (take w' (repeat '-')))
        return (fb', co', ci', ch, pr, sz')
    else
        return te


focusFileBox :: TextEditor -> IO TextEditor
focusFileBox te = do
    te@(fb, co, ci, ch, pr, sz) <- dolayout te
    paint PointChanged fb
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
            
        Just k -> do
            fb <- updateTextBox k fb
            focusFileBox (fb, co, ci, ch, pr, sz)
            
        Nothing ->
            focusFileBox te


uphistory :: HistoryCursor -> (String, HistoryCursor)
uphistory (cz :< c, Here, cs) = (c, (cz, Here, c : cs))
uphistory cur = ([], cur)


downhistory :: HistoryCursor -> (String, HistoryCursor)
downhistory (cz, Here, c : cs) = (c, (cz :< c, Here, cs))
downhistory cur = ([], cur)


focusConsole :: TextEditor -> IO TextEditor
focusConsole te = do
    te@(fb, co, ci, ch, pr, sz) <- dolayout te
    paint PointChanged ci
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
        
        Just Return -> do
            let [cmd] = getLines ci
                ci' = clear ci
                
                output = case parseFull parseExpr cmd of
                    Right expr -> show (eval pr [] expr)
                    Left err -> err
                    
                addh (cz, Here, cs) = (cz :< cmd, Here, cs)
                ch' = addh ch
            
            paint LineChanged ci'            
            co' <- appendAndScroll (lines output) co
            focusConsole (fb, co', ci', ch', pr, sz)
        
        Just (ArrowKey Normal UpArrow) -> do
            let (cmd, ch') = uphistory ch
            ci' <- setLines [cmd] ci
            focusConsole (fb, co, ci', ch', pr, sz)
            
        Just (ArrowKey Normal DownArrow) -> do
            let (cmd, ch') = downhistory ch
            ci' <- setLines [cmd] ci
            focusConsole (fb, co, ci', ch', pr, sz)
                
        Just k -> do
            ci <- updateTextBox k ci
            focusConsole (fb, co, ci, ch, pr, sz)
        
        Nothing ->
            focusFileBox te
            

focusOutput :: TextEditor -> IO TextEditor
focusOutput te = do
    te@(fb, co, ci, ch, pr, sz) <- dolayout te
    paint PointChanged co
    
    key <- getKey
    
    let handle k = do
        co' <- updateTextBox k co
        focusOutput (fb, co', ci, ch, pr, sz)
    
    case key of
        Just Escape -> return te
        Just k@(ArrowKey _ _) -> handle k
        Just End -> handle End
        Just Home -> handle Home
        _ -> focusOutput te
            
            
compile :: TextEditor -> IO TextEditor
compile (fb, co, ci, ch, pr, sz)
    = case parseFull parseProg (getText fb) of
        Left e -> do
            co' <- appendAndScroll (lines e) co
            return (fb, co', ci, ch, [], sz)
        
        Right p -> do
            co' <- appendAndScroll ["Compile successful."] co
            return (fb, co', ci, ch, p, sz)


mainLoop :: TextEditor -> IO TextEditor
mainLoop te = do
    te <- dolayout te
    curs_set 0
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
       
        Just (CharKey 'f') -> do
            curs_set 1
            te <- focusFileBox te
            mainLoop te
        
        Just (CharKey 'c') -> do
            curs_set 1
            te <- focusConsole te
            mainLoop te
            
        Just (CharKey 'o') -> do
            curs_set 1
            te <- focusOutput te
            mainLoop te
            
        Just (FunctionKey 6) -> do
            te <- compile te
            mainLoop te
            
        _ ->
            mainLoop te
        

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    xs <- getArgs
    file <- case xs of
        [] -> return ""
        (x : _) -> readFile x
    initscr
    clearScreen

    mainLoop (makeTextBox (lines file), makeTextBox [], makeTextBox [], (B0, Here, []), [], (0,0))
    endwin
