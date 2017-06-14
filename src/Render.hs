module Render
    ( render
    ) where

import Base
import Graphics.Vty

render :: GameState -> Picture
render (GameState snake dir foods (height, width) score) =
    let parts =
            [
            let st = if i == 0
                    then string (defAttr `withForeColor` (rgbColor 255 128 0)) ":"
                    else char (defAttr `withForeColor` green) $ getCh snake i
            in translate (x+1) (y+2) st
            | (i, Pos y x) <- zipWith (,) [0..] snake
            ]
        foods' =
            [
            let (ch, (r, g, b)) = foodRender fType
            in translate (x+1) (y+2) $ string (defAttr `withForeColor` (rgbColor r g b)) [ch]
            | (Pos y x, fType) <- foods
            ]
        horizLine = string defAttr $ "+" ++ replicate width '-' ++ "+"
        vertLine = vertCat $ replicate height (string defAttr $ "|" ++ replicate width ' ' ++ "|")
        border = translate 0 1 $ horizLine <-> vertLine <-> horizLine
        score' = string (defAttr `withForeColor` (rgbColor 127 255 0)) $ "Score: " ++ show score

    in picForLayers $ parts ++ foods' ++ [score', border]


render (Dead score) =
    picForImage $ vertCat $ map (horizCat . map (\(text, col) -> string (defAttr `withForeColor` col) text)) $ deadLines score

getCh :: Snake -> Int -> Char
getCh snake 0 = getCh snake 1
getCh snake i =
    let current = snake !! i
        before = snake !! (i - 1)
        beforeDiff = current `vSub` before
        next = if i == length snake - 1
            then Nothing -- current `vAdd` beforeDiff
            else Just $ snake !! (i + 1)
        nextDiff = fmap (current `vSub`) next

        beforeDiff' = getDir beforeDiff
        nextDiff' = fmap getDir nextDiff
        cmap = if use_ascii 
                then "|-\\\\//|-|-"
                else "┃━┓┗┛┏╻╸╹╺"
    in case (beforeDiff', nextDiff') of
        (0, Just 2) -> cmap !! 0
        (2, Just 0) -> cmap !! 0
        (1, Just 3) -> cmap !! 1
        (3, Just 1) -> cmap !! 1
        (0, Just 1) -> cmap !! 2
        (1, Just 0) -> cmap !! 2
        (2, Just 3) -> cmap !! 3
        (3, Just 2) -> cmap !! 3
        (2, Just 1) -> cmap !! 4
        (1, Just 2) -> cmap !! 4
        (3, Just 0) -> cmap !! 5
        (0, Just 3) -> cmap !! 5
        (0, Nothing) -> cmap !! 6
        (1, Nothing) -> cmap !! 7
        (2, Nothing) -> cmap !! 8
        (3, Nothing) -> cmap !! 9
        _ -> '+'

getDir :: (Num a, Eq a) => Pos a -> Int
getDir x =
    case x of
        Pos (-1) 0 -> 0
        Pos 0 1 -> 1
        Pos 1 0 -> 2
        Pos 0 (-1) -> 3
        _ -> -1

deadLines :: (Show a) => a -> [[(String, Color)]]
deadLines score =
    [ [("You died :(", red)]
    , [(" ", white)]
    , [(" ", white)]
    , [("Score: ", rgbColor 127 255 0), (show score, rgbColor 0 255 0)]
    , [("Press ", green), ("'A'", rgbColor 255 255 0), (" to play again!", green)]
    ]

