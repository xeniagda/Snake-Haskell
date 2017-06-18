module Base where

import Graphics.Vty.Input
import Data.Char

unicodeBlocks = "▪┃━┓┗┛┏╻╸╹╺"
asciiBlocks   = "*|-\\\\//v<^>"

data Pos a
    = Pos a a
    deriving (Show, Eq)

vAdd :: (Num a) => Pos a -> Pos a -> Pos a
vAdd (Pos y1 x1) (Pos y2 x2) = Pos (y1 + y2) (x1 + x2)

vSub :: (Num a) => Pos a -> Pos a -> Pos a
vSub (Pos y1 x1) (Pos y2 x2) = Pos (y1 - y2) (x1 - x2)

sMul :: (Num a) => Pos a -> a -> Pos a
sMul (Pos y x) a = Pos (y * a) (x * a)

data Foodtype
    = Apple
    | Lemon
    | Raspberry
    | Blueberry
    deriving (Show, Eq, Enum)

foodRender :: Foodtype -> (Char, (Int, Int, Int))
foodRender Apple = ('#', (255, 0, 0))
foodRender Lemon = ('#', (255, 255, 0))
foodRender Raspberry = ('%', (255, 128, 128))
foodRender Blueberry = ('%', (0, 128, 255))

type Snake = [Pos Int]
type Direction = Pos Int
data Food
    = Food  { getPos :: Pos Int
            , getType :: Foodtype
            , getTimeLeft :: Float
            , getDyingSpeed :: Float
            }
    deriving (Show, Eq)
type Score = Integer

data PlayingState
    = PlayingState  { getSnake :: Snake
                    , getDirection :: Direction
                    , getFood :: [Food]
                    , getScore :: Score
                    }
    deriving (Show, Eq )

data Settings
    = Settings  { getCMap :: String
                , getSize :: (Int, Int)
                , getDiff :: Int -- 0 - 10, 0 = Easy, 10 = Hard
                , getStartState :: Maybe StartState
                }
    deriving (Eq)

data StartState
    = StartState    { getCurrentSetting :: Int
                    , getSettings :: [Setting]
                    }
    deriving (Eq)

data Setting
    = Setting   { getSettingName :: String
                , getValue :: Value
                , set :: (Settings -> Value -> Settings)
                , sUpdate :: (Setting -> Event -> Value)
                }
    | StartGameButton

instance Eq Setting where
    StartGameButton == StartGameButton = True
    StartGameButton == _ = False
    _ == StartGameButton = False
    s1 == s2 =
                getSettingName s1 == getSettingName s2
             && getValue s1 == getValue s2


data Value
    = VInt Int
    | VBool Bool
    deriving (Eq)

data GameState
    = Start StartState
    | Playing PlayingState Settings
    | Dead Score Settings
    deriving (Eq)

defaultSettings =
    Settings
        { getCMap = unicodeBlocks
        , getSize = (30, 30)
        , getDiff = 5
        , getStartState = Nothing
        }

defaultGame =
    PlayingState
        { getSnake = [Pos 0 0, Pos 0 0]
        , getDirection = (Pos 0 1)
        , getFood = []
        , getScore = 0
        }

defaultStartState =
    StartState
        { getCurrentSetting = 0
        , getSettings =
            [ Setting
                { getSettingName = "Difficulty"
                , getValue = VInt 5
                , set = (\settings (VInt diff) -> settings {getDiff = diff})
                , sUpdate = updateInt
                }
            , Setting
                { getSettingName = "Game width"
                , getValue = VInt 30
                , set = (\settings (VInt width) -> settings { getSize = (fst $ getSize settings, width) })
                , sUpdate = updateInt
                }
            , Setting
                { getSettingName = "Game height"
                , getValue = VInt 30
                , set = (\settings (VInt height) -> settings { getSize = (height, snd $ getSize settings) })
                , sUpdate = updateInt
                }
            , Setting
                { getSettingName = "Use unicode blocks"
                , getValue = VBool True
                , set = (\settings (VBool unic) -> settings { getCMap = if unic then unicodeBlocks else asciiBlocks })
                , sUpdate = updateBool
                }
            , StartGameButton
            ]
        }

updateInt :: Setting -> Event -> Value
updateInt setting (EvKey KLeft []) =
    let VInt val = getValue setting
    in VInt $ val - 1
updateInt setting (EvKey KRight []) =
    let VInt val = getValue setting
    in VInt $ val + 1
updateInt setting (EvKey KBS []) =
    let VInt val = getValue setting
    in VInt $ val `quot` 10
updateInt setting (EvKey (KChar k) []) =
    let ccode = ord k - ord '0'
        VInt val = getValue setting
    in if ccode >= 0 && ccode <= 9
        then VInt $ val * 10 + ccode
        else VInt val

updateInt s _ = getValue s

updateBool :: Setting -> Event -> Value
updateBool setting (EvKey k [])
    | k `elem` [KLeft, KRight, KChar ' '] =
        let VBool val = getValue setting
        in VBool $ not val

updateBool s _ = getValue s


makeSettings :: Settings -> [Setting] -> Settings
makeSettings s [] = s
makeSettings s (StartGameButton:other) = makeSettings s other
makeSettings s (setting:other) = makeSettings ((set setting) s (getValue setting)) other

