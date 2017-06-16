module Base where

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
type GameSize = (Int, Int)
type Score = Integer

data PlayingState
    = PlayingState  { getSnake :: Snake
                    , getDirection :: Direction
                    , getFood :: [Food]
                    , getSize :: GameSize
                    , getScore :: Score
                    }
    deriving (Show, Eq )

data Settings
    = Settings  { getCMap :: String }
    deriving (Show, Eq)

data GameState
    = Playing PlayingState Settings
    | Dead Score Settings
    deriving (Show, Eq)

defaultSettings = Settings { getCMap = "┃━┓┗┛┏╻╸╹╺" }
gameStart = Playing 
                (PlayingState { getSnake = [Pos 0 0, Pos 0 (-1)], getDirection = (Pos 0 1), getFood = [], getSize = (30, 30), getScore = 0}) 
                defaultSettings
