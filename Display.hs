
module Display where

import Foreign
import Foreign.C (CInt(..))

foreign import ccall
  initscr :: IO () 

foreign import ccall "endwin"
  endwin :: IO CInt

foreign import ccall "refresh"
  refresh :: IO CInt

foreign import ccall "&LINES"
  linesPtr :: Ptr CInt

foreign import ccall "&COLS"
  colsPtr :: Ptr CInt
  
type Point = (Int, Int)
type Size = (Int, Int)

screenSize :: IO (Int, Int)
screenSize = do
    lnes <- peek linesPtr
    cols <- peek colsPtr
    return (fromIntegral cols, fromIntegral lnes)
