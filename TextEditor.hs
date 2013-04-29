
import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.Environment


focus :: TextBox -> IO TextBox
focus tb = do
    refresh
    key <- getKey
    
    case key of
        Nothing -> focus tb
        Just Escape -> return tb
        Just k -> do
            size <- screenSize
            let ((view, cursor@(x,y), damage), tb') = runAction (handleKeyAction k size) tb
            
            case damage of
                LotsChanged -> do
                    resetCursor
                    mapM_ putStr view
                
                LineChanged -> do
                    cursorToPoint (0, y)
                    putStr (view !! y)

                _ -> return ()
            
            cursorToPoint cursor
            focus tb'
            

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    xs <- getArgs
    file <- case xs of
        [] -> return ""
        (x : _) -> readFile x
    initscr
    size <- screenSize
    let tb = makeTextBox (lines file) size
    clearScreen
    focus tb
    endwin
