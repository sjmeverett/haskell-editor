
import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.Environment

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


run :: TextEditor -> IO ()
run te = do
    te'@(fb, co, ci, sz) <- dolayout te
    key <- getKey
    
    case key of
        Just Escape ->
            return ()
            
        Just k -> do
            fb' <- updateTextBox k fb
            run (fb', co, ci, sz)
            
        Nothing ->
            run te'
            
            
--    where
--        focusFileBox (TextEditor { fileBox = fb, consoleOutput = co, consoleInput = ci, size = sz })
--            = TextEditor { fileBox = focus fb, consoleOutput = co, consoleInput = ci, size = sz })


main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    xs <- getArgs
    file <- case xs of
        [] -> return ""
        (x : _) -> readFile x
    initscr
    clearScreen
    
    let te = (makeTextBox (lines file), makeTextBox ["Interactive console. Type help for help.", ""], makeTextBox [], (0,0))
    
    run te
    endwin
