
import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.Environment


focus :: TextBox -> IO ()
focus tb = do
    key <- getKey
    
    case key of
        Nothing -> focus tb
        Just Escape -> return ()
        Just k -> do
            size <- screenSize
            let ((view, cursor, damage), tb') = runAction (handleKeyAction k size) tb
            
            case damage of
                _ -> do
                    clearScreen
                    resetCursor
                    mapM_ putStr view
            
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
    focus tb
    endwin
