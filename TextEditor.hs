
module Main where

import Keys
import Display
import TextBox
import ANSIEscapes
import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import FoulParser
import FOUL

type HistoryCursor = Cursor String Here

{-A TextEditor has a file edit box, a console output, a console input, a command history, a compiled program, a size and a file path-}
type TextEditor = (TextBox, TextBox, TextBox, HistoryCursor, Prog, Size, String)


dolayout :: TextEditor -> IO TextEditor
dolayout te@(fb, co, ci, ch, pr, sz@(w, h), path) = do
    refresh
    sz'@(w', h') <- screenSize
    
    let (_, fb') = runAction (relocate (0, 0) (w', fbheight)) fb
        (_, co') = runAction (relocate (0, fbheight + 1) (w', coheight)) co
        (_, ci') = runAction (relocate (0, fbheight + coheight + 1)(w', 1)) ci
        fbheight = h' - coheight - 2
        coheight = 6
    
    if sz' /= sz then do
        paint LotsChanged fb'
        paint LotsChanged co'
        paint LotsChanged ci'
        cursorToPoint (0, fbheight)
        putStr (yellow (take w' (repeat '-')))
        return (fb', co', ci', ch, pr, sz', path)
    else
        return te


focusFileBox :: TextEditor -> IO TextEditor
focusFileBox te = do
    te@(fb, co, ci, ch, pr, sz, path) <- dolayout te
    paint PointChanged fb
    
    key <- getKey
    
    case key of
        Just Escape ->
            return te
            
        Just k -> do
            fb <- updateTextBox k fb
            focusFileBox (fb, co, ci, ch, pr, sz, path)
            
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
    te@(fb, co, ci, ch, pr, sz, path) <- dolayout te
    paint PointChanged ci
    
    key <- getKey
    
    case key of
        Just Escape -> do
            let ci' = clear ci
            paint LineChanged ci'
            return (fb, co, ci', ch, pr, sz, path)
        
        Just Return -> do
            let [cmd] = getLines ci
                ci' = clear ci
                
                output = case parseFull parseExpr cmd of
                    Right expr -> case eval pr [] expr of
                        CanFail (Left e) -> e
                        CanFail (Right val) -> show val
                    Left err -> err
                    
                addh (cz, Here, cs) = (cz :< cmd, Here, cs)
                ch' = addh ch
            
            paint LineChanged ci'            
            co' <- appendAndScroll (lines output) co
            focusConsole (fb, co', ci', ch', pr, sz, path)
        
        Just (ArrowKey Normal UpArrow) -> do
            let (cmd, ch') = uphistory ch
            ci' <- setLines [cmd] ci
            focusConsole (fb, co, ci', ch', pr, sz, path)
            
        Just (ArrowKey Normal DownArrow) -> do
            let (cmd, ch') = downhistory ch
            ci' <- setLines [cmd] ci
            focusConsole (fb, co, ci', ch', pr, sz, path)
                
        Just k -> do
            ci <- updateTextBox k ci
            focusConsole (fb, co, ci, ch, pr, sz, path)
        
        Nothing ->
            focusFileBox te
            

focusOutput :: TextEditor -> IO TextEditor
focusOutput te = do
    te@(fb, co, ci, ch, pr, sz, path) <- dolayout te
    paint PointChanged co
    
    key <- getKey
    
    let handle k = do
        co' <- updateTextBox k co
        focusOutput (fb, co', ci, ch, pr, sz, path)
    
    case key of
        Just Escape -> return te
        Just k@(ArrowKey _ _) -> handle k
        Just End -> handle End
        Just Home -> handle Home
        _ -> focusOutput te
            
            
compile :: TextEditor -> IO TextEditor
compile (fb, co, ci, ch, pr, sz, path)
    = case parseFull parseProg (getText fb) of
        Left e -> do
            co' <- appendAndScroll (lines e) co
            return (fb, co', ci, ch, [], sz, path)
        
        Right p -> do
            co' <- appendAndScroll ["Compile successful."] co
            return (fb, co', ci, ch, p, sz, path)


save :: TextEditor -> IO TextEditor
save (fb, co, ci, ch, pr, sz, path) = do
    ci' <- setLines [path] ci
    inner (fb, co, ci', ch, pr, sz, path)
    
    where
        inner te = do
            te@(fb, co, ci, ch, pr, sz, path) <- dolayout te
            paint PointChanged ci
            
            key <- getKey
            
            case key of
                Just Escape ->
                    return te
                
                Just Return -> do
                    let [path'] = getLines ci
                        ci' = clear ci
                    
                    paint LineChanged ci'
                    writeFile path' (getText fb)
                    
                    return (fb, co, ci', ch, pr, sz, path')
            
            


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
        
        Just (CharKey 's') -> do
            curs_set 1
            te <- save te
            mainLoop te
        
        _ ->
            mainLoop te
        

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

    args <- getArgs
    
    let path = case args of
            [] -> ""
            (x : _) -> x
    
    exists <- doesFileExist path
    file <- if path == "" || not exists then return "" else readFile path

    initscr
    clearScreen

    te <- compile (makeTextBox (lines file), makeTextBox [], makeTextBox [], (B0, Here, []), [], (0,0), path)
    mainLoop te
    endwin
