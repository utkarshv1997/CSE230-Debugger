{-# LANGUAGE TemplateHaskell #-}
module UI(ui) where

import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.AttrMap
  ( attrMap, AttrMap
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  )
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import Data.Set hiding (map)
import System.Environment
import System.Exit
import Lens.Micro.TH
import Brick
import Graphics.Vty
import Control.Lens
import Brick.Widgets.Border.Style
import System.IO

data Name =     CodeView 
            |   StackView
            |   VariablesView
            |   CommandsView
            deriving (Ord, Show, Eq)

data ProgState = ProgState {
    _focusRing :: F.FocusRing Name,
    codeLines :: [(String, Int)], -- (line of code, line number)
    selectedElement :: Int, -- The currently selected element in the CodeView.
    breakpoints :: Set Int, -- contains "breakpointed" line numbers
    commandsViewContent :: String -- This is what goes inside the "Help" View
}

Lens.Micro.TH.makeLenses ''ProgState

drawUi :: ProgState -> [Widget Name]
drawUi ps = case F.focusGetCurrent (ps^.focusRing) of
                Just CodeView -> [buildViewOf 0]
                Just StackView -> [buildViewOf 1]
                Just VariablesView -> [buildViewOf 2]
                Just CommandsView -> [buildViewOf 3]
                Nothing -> error "Illegal State Error"
                where
                    svw = str "This will have the Stack contents"
                    vvw = str "This will have the Variables and their values"
                    comvw = str $ commandsViewContent ps
                    buildViewOf = buildViewUI ps svw vvw comvw

buildViewUI :: ProgState -> Widget Name -> Widget Name -> Widget Name -> Int -> Widget Name
buildViewUI ps stackViewWidget varViewWidget comViewWidget viewType = 
    C.center $ withBorderStyle unicodeBold $ B.borderWithLabel headLabel $ hBox [ vBox [ withBorderStyle unicodeBold $ B.hBorderWithLabel (withAttr headerLabelScheme $ str "Code View"), 
        withAttr codePanelBg codePanel ], withBorderStyle unicodeBold B.vBorder, miscPanel]
    where
        headLabel = withAttr headerLabelScheme $ str "THE DELTA DEBUGGER FOR EPSILON"
        codePanel = viewport CodeView Both $ vBox $ assembleCodeViewWidgets ps $ viewType == 0
        miscPanel = vBox [ 
                            withBorderStyle unicodeBold $ B.hBorderWithLabel (withAttr headerLabelScheme $ str "Stack View"),
                            withAttr (if viewType == 1 then miscPanelSelectedScheme else miscPanelScheme) $ viewport StackView Both stackViewWidget,
                            withBorderStyle unicodeBold $ B.hBorderWithLabel (withAttr headerLabelScheme $ str "Variables View"),
                            withAttr (if viewType == 2 then miscPanelSelectedScheme else miscPanelScheme) $ viewport VariablesView Both varViewWidget,
                            withBorderStyle unicodeBold $ B.hBorderWithLabel (withAttr headerLabelScheme $ str "Help View"),
                            withAttr (if viewType == 3 then miscPanelSelectedScheme else miscPanelScheme) $ viewport CommandsView Both comViewWidget
                         ]

assembleCodeViewWidgets :: ProgState -> Bool -> [Widget n]
assembleCodeViewWidgets ps isCodeView = map (\ (codeline, line) -> 
                            if member line (breakpoints ps) then 
                                withAttr breakpointScheme $ str $ show line ++ (if line < 10 then ".  " else ". ") ++ codeline
                            else
                                if line == selectedElement ps && isCodeView then
                                    withAttr codePanelSelectedScheme $ str $ show line ++ (if line < 10 then ".  " else ". ") ++ codeline
                                else     
                                    withAttr codePanelScheme $ str $ show line ++ (if line < 10 then ".  " else ". ") ++ codeline
                         ) 
                     (codeLines ps)

cvScroll :: M.ViewportScroll Name
cvScroll = M.viewportScroll CodeView

svScroll :: M.ViewportScroll Name
svScroll = M.viewportScroll StackView

vvScroll :: M.ViewportScroll Name
vvScroll = M.viewportScroll VariablesView

covScroll :: M.ViewportScroll Name
covScroll = M.viewportScroll CommandsView

appCursor :: ProgState -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

-- We may need, if we want of course, to check for conditions that are not valid inputs etc.
appEvent :: ProgState -> T.BrickEvent Name e -> T.EventM Name (T.Next ProgState)
appEvent ps (T.VtyEvent ev) = case ev of 
    V.EvKey V.KEsc [] -> M.halt ps
    V.EvKey (V.KChar '\t') [] -> M.continue $ ps & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ ps & focusRing %~ F.focusPrev
    V.EvKey V.KDown [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.vScrollBy cvScroll 1 >> M.continue (updateSelectedElement ps 1)
        Just StackView -> M.vScrollBy svScroll 1 >> M.continue ps
        Just VariablesView -> M.vScrollBy vvScroll 1 >> M.continue ps
        Just CommandsView -> M.vScrollBy covScroll 1 >> M.continue ps
        Nothing -> M.continue ps
    V.EvKey V.KUp [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.vScrollBy cvScroll (-1) >> M.continue (updateSelectedElement ps (-1))
        Just StackView -> M.vScrollBy svScroll (-1) >> M.continue ps
        Just VariablesView -> M.vScrollBy vvScroll (-1) >> M.continue ps
        Just CommandsView -> M.vScrollBy covScroll (-1) >> M.continue ps
        Nothing -> M.continue ps
    V.EvKey V.KLeft [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.hScrollBy cvScroll (-1) >> M.continue ps
        Just StackView -> M.hScrollBy svScroll (-1) >> M.continue ps
        Just VariablesView -> M.hScrollBy vvScroll (-1) >> M.continue ps
        Just CommandsView -> M.hScrollBy covScroll (-1) >> M.continue ps
        Nothing -> M.continue ps
    V.EvKey V.KRight [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.hScrollBy cvScroll 1 >> M.continue ps
        Just StackView -> M.hScrollBy svScroll 1 >> M.continue ps
        Just VariablesView -> M.hScrollBy vvScroll 1 >> M.continue ps
        Just CommandsView -> M.hScrollBy covScroll 1 >> M.continue ps
        Nothing -> M.continue ps
    V.EvMouseDown {} -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.vScrollBy cvScroll 1 >> M.continue (updateSelectedElement ps 1)
        Just StackView -> M.vScrollBy svScroll 1 >> M.continue ps
        Just VariablesView -> M.vScrollBy vvScroll 1 >> M.continue ps
        Just CommandsView -> M.vScrollBy covScroll 1 >> M.continue ps
        Nothing -> M.continue ps
    V.EvMouseUp {} -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.vScrollBy cvScroll (-1) >> M.continue (updateSelectedElement ps (-1))
        Just StackView -> M.vScrollBy svScroll (-1) >> M.continue ps
        Just VariablesView -> M.vScrollBy vvScroll (-1) >> M.continue ps
        Just CommandsView -> M.vScrollBy covScroll (-1) >> M.continue ps
        Nothing -> M.continue ps
    V.EvKey (V.KChar 'b') [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> M.continue $ updateBreakPointSet ps
        _ -> M.continue ps
    V.EvKey (V.KChar 'r') [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> error "This is where the control gets transferred to the AST editor method where we insert 'breaks' in the AST and call"
                                "the evaluator/interpreter for the first time and update the ProgState"
        _ -> M.continue ps
    V.EvKey (V.KChar 'n') [] -> case F.focusGetCurrent (ps^.focusRing) of
        Just CodeView -> error "Execute the evaluator till the next breakpoint/end."
        _ -> M.continue ps
    _ -> M.continue ps
appEvent ps _ = M.continue ps

updateBreakPointSet :: ProgState -> ProgState
updateBreakPointSet ps = ProgState {
                                    _focusRing = ps^.focusRing, 
                                    codeLines = codeLines ps, 
                                    selectedElement = selectedElement ps,
                                    breakpoints = updatedBPSet,
                                    commandsViewContent = commandsViewContent ps
                                }
                                where updatedBPSet = if member (selectedElement ps) (breakpoints ps) then
                                                        delete (selectedElement ps) (breakpoints ps)
                                                     else 
                                                        insert (selectedElement ps) (breakpoints ps)

updateSelectedElement :: ProgState -> Int -> ProgState
updateSelectedElement ps c = ProgState {
                                    _focusRing = ps^.focusRing, 
                                    codeLines = codeLines ps, 
                                    selectedElement = min (max 1 (selectedElement ps + c)) maxLine,
                                    breakpoints = breakpoints ps,
                                    commandsViewContent = commandsViewContent ps
                                }
                                where maxLine = snd $ last (codeLines ps)


app :: M.App ProgState e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const attributes
          , M.appChooseCursor = appCursor
          }

codePanelScheme, headerLabelScheme, codePanelBg, codePanelSelectedScheme, miscPanelScheme :: AttrName
miscPanelSelectedScheme, breakpointScheme :: AttrName
codePanelScheme = attrName "code-panel-scheme"
codePanelBg = attrName "code-panel-bg"
codePanelSelectedScheme = attrName "code-panel-selected-scheme"
miscPanelScheme = attrName "misc-panel-scheme"
miscPanelSelectedScheme = attrName "misc-panel-selected-scheme"
breakpointScheme = attrName "breakpoint-scheme"
headerLabelScheme = attrName "header-label-scheme"

attributes :: AttrMap
attributes = attrMap defAttr [
                (codePanelScheme, white `on` blue),
                (codePanelBg, bg blue),
                (codePanelSelectedScheme, black `on` yellow),
                (miscPanelSelectedScheme, withStyle (red `on` white) bold),
                (miscPanelScheme, yellow `on` black),
                (breakpointScheme, white `on` red),
                (headerLabelScheme, withStyle (withStyle (blue `on` white) bold) underline)
             ]

constructCodeStructure :: [String] -> Int -> [(String, Int)]
constructCodeStructure [] _ = [] 
constructCodeStructure (l : ls) c = (l, c) : constructCodeStructure ls (c + 1)

initState :: [String] -> String -> ProgState
initState codeLines commands = ProgState {
                                    _focusRing = F.focusRing [CodeView, StackView, VariablesView, CommandsView], 
                                    codeLines = constructCodeStructure codeLines 1, 
                                    selectedElement = 1,
                                    breakpoints = empty,
                                    commandsViewContent = commands
                                }
                      
ui :: IO ()
ui = do
        args <- getArgs
        case args of
            [codeFileName, commandsFileName] -> do
                codeLines <- lines <$> readFile codeFileName
                commandsViewContent <- lines <$> readFile commandsFileName
                void $ M.defaultMain app $ initState codeLines $ unlines commandsViewContent
                exitSuccess
            _ -> do
                putStr "Please provide a filename to be opened."
                exitFailure