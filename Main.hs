import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Graphics.Vty.Attributes

-- 定义应用程序的状态
data AppState = AppState

-- 定义用户界面
app :: App AppState e ()
app = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const theMap
}

-- 定义 UI 属性
theMap :: AttrMap
theMap = attrMap defAttr
    [ ("button", white `on` blue)
    , ("activeButton", blue `on` white)
    ]

-- 渲染用户界面
drawUI :: AppState -> [Widget ()]
drawUI _ = [ui]
    where
        ui = center $ vBox [ button "Game 1"
                           , button "Game 2"
                           , button "Game 3"
                           ]
        button label = withAttr "button" $ hLimit 20 $ vLimit 1 $ 
                       vCenter $ hCenter $ str label

-- 处理事件
handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s
handleEvent s _ = continue s

-- 主函数
main :: IO ()
main = defaultMain app AppState
