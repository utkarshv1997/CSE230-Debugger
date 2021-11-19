module Main where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty (defAttr)
import Graphics.Vty.Input
import WEditorBrick.WrappingEditor  -- For the wrapping editor Brick widget.
import WEditor.LineWrap
import WEditor.Document
import Brick
import Brick.Widgets.Core (strWrap)
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Data.Set hiding (map)


-- Delegate most events to a single handler. If you get annoyed by scrolling
-- past the last line, you can apply mapEditor viewerFillAction before continue.
handleEventsWith :: (t -> Event -> EventM n1 t) -> t -> BrickEvent n2 e -> EventM n1 (Next t)
handleEventsWith _ x (VtyEvent (EvKey KEsc [])) = halt x
handleEventsWith handler x (VtyEvent e) = continue =<< handler x e

data ProgState = ProgState {
    codeLines :: [(String, Int)], -- (line of code, line number)
    breakpoints :: Set Int -- contains "breakpointed" line numbers
}

generalScheme, breakpointScheme :: AttrName
generalScheme = attrName "general-scheme"
breakpointScheme = attrName "breakpoint-scheme"

-- The Application interface
app :: App ProgState e Int
app = App {
  appStartEvent = return, -- We would probably create the primary (without any breakpoints) AST here
  appDraw = assembleWidgets,
  appHandleEvent = handleEvent,
  appChooseCursor = neverShowCursor,
  appAttrMap = const (attrMap defAttr [
                  (generalScheme, white `on` blue),
                  (breakpointScheme, white `on` red)
               ])
}

handleEvent :: ProgState -> BrickEvent Int e -> EventM Int (Next ProgState)
handleEvent ps (VtyEvent (V.EvKey V.KUp []))     = error "fill me"
handleEvent ps (VtyEvent (V.EvKey V.KDown []))   = error "fill me"
handleEvent ps (VtyEvent (V.EvKey V.KLeft []))   = error "fill me"
handleEvent ps (VtyEvent (V.EvKey V.KRight []))  = error "fill me"
handleEvent ps (VtyEvent (V.EvKey V.KEsc []))    = halt ps
handleEvent ps _                                 = continue ps

assembleWidgets :: ProgState -> [Widget n]
assembleWidgets ps = map (\ (codeline, line) -> 
                                        if member line (breakpoints ps) then 
                                          withAttr breakpointScheme $ strWrap codeline
                                        else 
                                          withAttr generalScheme $ strWrap codeline
                         ) 
                     (codeLines ps)

initState :: [String] -> Int -> [(String, Int)]
initState [] _ = [] 
initState (l : ls) c = (l, c) : initState ls (c + 1)

readFileContents :: FilePath -> IO String
readFileContents f = do
  contents <- lines <$> readFile f
  return (unlines contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
      [fileName] -> do
        codeLines <- lines <$> readFile fileName
        finalState <- defaultMain app (ProgState {codeLines = initState codeLines 1, breakpoints = empty})
        -- Pass on the final state to the function that handles the AST insertion of breakpoints
        exitSuccess
      _ -> do
        putStr "Please provide a filename to be opened."
        exitFailure