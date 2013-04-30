
import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.Environment
import FoulParser

type HistoryCursor = Cursor String Here

{-A TextEditor has a file edit box, a console output, a console input, a command history, and a size-}
type TextEditor = (TextBox, TextBox, TextBox, HistoryCursor, Size)


dolayout :: TextEditor -> IO TextEditor
dolayout te@(fb, co, ci, ch, sz@(w, h)) = do
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
        return (fb', co', ci', ch, sz')
    else
        return te


focusFileBox :: TextEditor -> IO TextEditor
focusFileBox te = do
    te@(fb, co, ci, ch, sz) <- dolayout te
    paint PointChanged fb
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
            
        Just k -> do
            fb <- updateTextBox k fb
            focusFileBox (fb, co, ci, ch, sz)
            
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
    te@(fb, co, ci, ch, sz) <- dolayout te
    paint PointChanged ci
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
        
        Just Return -> do
            let [cmd] = getLines ci
                ci' = clear ci
                output = in2out cmd
                addh (cz, Here, cs) = (cz :< cmd, Here, cs)
                ch' = addh ch
            
            paint LineChanged ci'            
            co' <- appendAndScroll (lines output) co
            focusConsole (fb, co', ci', ch', sz)
        
        Just (ArrowKey Normal UpArrow) -> do
            let (cmd, ch') = uphistory ch
            ci' <- setLines [cmd] ci
            focusConsole (fb, co, ci', ch', sz)
            
        Just (ArrowKey Normal DownArrow) -> do
            let (cmd, ch') = downhistory ch
            ci' <- setLines [cmd] ci
            focusConsole (fb, co, ci', ch', sz)
                
        Just k -> do
            ci <- updateTextBox k ci
            focusConsole (fb, co, ci, ch, sz)
        
        Nothing ->
            focusFileBox te
            


mainLoop :: TextEditor -> IO TextEditor
mainLoop te = do
    te <- dolayout te
    curs_set 0
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
       
        Just Return -> do
            curs_set 1
            te <- focusFileBox te
            mainLoop te
        
        Just (CharKey 'c') -> do
            curs_set 1
            te <- focusConsole te
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

    mainLoop (makeTextBox (lines file), makeTextBox [], makeTextBox [], (B0, Here, []), (0,0))
    endwin
