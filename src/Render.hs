module Render
    ( render
    ) where

import Base
import Graphics.Vty

render :: GameState -> Picture
render (Playing state settings) =
    let parts =
            [
                let st = if i == 0
                        then char (defAttr `withForeColor` (rgbColor 255 128 0)) (getCMap settings !! 0)
                        else char (defAttr `withForeColor` green) $ getCh (getCMap settings) (getSnake state) i
            in translate (x+1) (y+2) st
            | (i, Pos y x) <- zipWith (,) [0..] $ getSnake state
            ]
        foods =
            [
                let (ch, (r, g, b)) = foodRender $ getType food
                    [r', g', b'] = map (toInteger . floor . (* getTimeLeft food) . fromIntegral) [r, g, b]
                    Pos y x = getPos food
                in translate (x+1) (y+2) $
                    string
                        (defAttr
                            `withForeColor` (rgbColor r' g' b')
                        )
                        [ch]
            | food <- getFood state
            ]
        (height, width) = getSize settings
        topLine = string defAttr $ [getCMap settings !! 6] ++ replicate width (getCMap settings !! 2) ++ [getCMap settings !! 3]
        vertLine = vertCat $ replicate height (string defAttr $ [getCMap settings !! 1] ++ replicate width ' ' ++ [getCMap settings !! 1])
        bottomLine = string defAttr $ [getCMap settings !! 4] ++ replicate width (getCMap settings !! 2) ++ [getCMap settings !! 5]

        border = translate 0 1 $ topLine <-> vertLine <-> bottomLine
        score = string (defAttr `withForeColor` (rgbColor 127 255 0)) $ "Score: " ++ show (getScore state)

    in picForLayers $ parts ++ foods ++ [score, border]

render (Dead score settings) =
    picForImage $ vertCat $ map (horizCat . map (\(text, col) -> string (defAttr `withForeColor` col) text)) $ deadLines $ Dead score settings

render (Start startState) =
    let title =
            string defAttr (replicate 7 ' ')
            <|>
            ( 
            string defAttr "  🐍🐍"
            <->
            string (defAttr `withStyle` underline `withStyle` bold `withForeColor` rgbColor 0 255 0) " SNAKE "
            <|>
            string defAttr " "
            )
        settings =
            zipWith
                (\i setting ->
                    let rendered = renderSetting setting (i == getCurrentSetting startState)
                        terminated = rendered <|> string defAttr " "
                    in translate 2 (i * 2 + 3) terminated
                ) [0..] (getSettings startState)
    in picForLayers $ title : settings

renderSetting :: Setting -> Bool -> Image
renderSetting StartGameButton active =
    string defAttr " " <|> string (getAttr active `withForeColor` green) " === Start === "

renderSetting sett active =
    let val =
            case (getValue sett) of
                VInt val -> show val
                VBool val -> show val
        before =
            if active 
                then "> "
                else "  "
    in string (getAttr active) (before ++ getSettingName sett ++ ": " ++ val)

getAttr :: Bool -> Attr
getAttr False = defAttr
getAttr True = defAttr `withStyle` bold `withStyle` underline `withForeColor` rgbColor 255 128 0

getCh :: String -> Snake -> Int -> Char
getCh cmap snake 0 = getCh cmap snake 1
getCh cmap snake i =
    let current = snake !! i
        before = snake !! (i - 1)
        beforeDiff = current `vSub` before
        next = if i == length snake - 1
            then Nothing -- current `vAdd` beforeDiff
            else Just $ snake !! (i + 1)
        nextDiff = fmap (current `vSub`) next

        beforeDiff' = getDir beforeDiff
        nextDiff' = fmap getDir nextDiff
    in case (beforeDiff', nextDiff') of
        (0, Just 2) -> cmap !! 1
        (2, Just 0) -> cmap !! 1
        (1, Just 3) -> cmap !! 2
        (3, Just 1) -> cmap !! 2
        (0, Just 1) -> cmap !! 3
        (1, Just 0) -> cmap !! 3
        (2, Just 3) -> cmap !! 4
        (3, Just 2) -> cmap !! 4
        (2, Just 1) -> cmap !! 5
        (1, Just 2) -> cmap !! 5
        (3, Just 0) -> cmap !! 6
        (0, Just 3) -> cmap !! 6
        (0, Nothing) -> cmap !! 7
        (1, Nothing) -> cmap !! 8
        (2, Nothing) -> cmap !! 9
        (3, Nothing) -> cmap !! 10
        _ -> '+'

getDir :: (Num a, Eq a) => Pos a -> Int
getDir x =
    case x of
        Pos (-1) 0 -> 0
        Pos 0 1 -> 1
        Pos 1 0 -> 2
        Pos 0 (-1) -> 3
        _ -> -1

deadLines :: GameState -> [[(String, Color)]]
deadLines (Dead score settings) =
    [ [("You died :(", red)]
    , [(" ", white)]
    , [("Difficulty: " ++ show (getDiff settings), white)]
    , [(" ", white)]
    , [("Score: ", rgbColor 127 255 0), (show score, rgbColor 0 255 0)]
    , [("Press ", green), ("'A'", rgbColor 255 255 0), (" to play again!", green)]
    ]

