
import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.Environment
import FoulParser

{-A TextEditor has a file edit box, a console output, a console input, and a size-}
type TextEditor = (TextBox, TextBox, TextBox, Size)


dolayout :: TextEditor -> IO TextEditor
dolayout te@(fb, co, ci, sz@(w, h)) = do
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
        return (fb', co', ci', sz')
    else
        return te


focusFileBox :: TextEditor -> IO TextEditor
focusFileBox te = do
    te@(fb, co, ci, sz) <- dolayout te
    paint PointChanged fb
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
            
        Just k -> do
            fb <- updateTextBox k fb
            focusFileBox (fb, co, ci, sz)
            
        Nothing ->
            focusFileBox te



focusConsole :: TextEditor -> IO TextEditor
focusConsole te = do
    te@(fb, co, ci, sz) <- dolayout te
    paint PointChanged ci
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
        
        Just Return -> do
            let [cmd] = getLines ci
                ci' = clear ci
                output = in2out cmd
            
            paint LineChanged ci'            
            co' <- appendAndScroll (lines output) co
            focusConsole (fb, co', ci', sz)
        
        Just k -> do
            ci <- updateTextBox k ci
            focusConsole (fb, co, ci, sz)
        
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

    mainLoop (makeTextBox (lines file), makeTextBox [], makeTextBox [], (0,0))
    endwin
