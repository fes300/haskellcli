{-# LANGUAGE CPP #-}

module Main where

import Lens.Micro ((^.))
import Lens.Micro.Mtl

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Graphics.Vty qualified as V

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (
  Widget,
 )
import Brick.Types qualified as T
import Brick.Util (fg, on)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (
  hLimit,
  str,
  vBox,
  vLimit,
  withAttr,
  (<+>),
 )
import Brick.Widgets.List qualified as L
import Data.Vector qualified as Vec

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case l ^. L.listSelectedL of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box =
      B.borderWithLabel label $
        hLimit 25 $
          vLimit 15 $
            L.renderList listDrawElement True l
    ui =
      C.vCenter $
        vBox
          [ C.hCenter box
          , str " "
          , C.hCenter $ str "Press +/- to add/remove list elements."
          , C.hCenter $ str "Press Esc to exit."
          ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () Char) ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '+') [] -> do
      els <- use L.listElementsL
      let el = nextElement els
          pos = Vec.length els
      modify $ L.listInsert pos el
    V.EvKey (V.KChar '-') [] -> do
      sel <- use L.listSelectedL
      case sel of
        Nothing -> pass
        Just i -> modify $ L.listRemove i
    V.EvKey V.KEsc [] -> M.halt
    ev -> L.handleListEvent ev
  where
    nextElement :: Vec.Vector Char -> Char
    nextElement v = fromMaybe '?' $ Vec.find (`Vec.notElem` v) (Vec.fromList ['a' .. 'z'])
appEvent _ = pass

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
   in C.hCenter $ str "Item " <+> selStr (show a)

initialState :: L.List () Char
initialState = L.list () (Vec.fromList ['a', 'b', 'c']) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `Brick.Util.on` V.blue)
    , (L.listSelectedAttr, V.blue `Brick.Util.on` V.white)
    , (customAttr, fg V.cyan)
    ]

theApp :: M.App (L.List () Char) e ()
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = pass
    , M.appAttrMap = const theMap
    }

main :: IO ()
main = void $ M.defaultMain theApp initialState
