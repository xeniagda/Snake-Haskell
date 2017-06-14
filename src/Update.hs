module Update
    ( gameUpdate
    ) where

import Graphics.Vty
import System.Random

import Base

gameUpdate :: GameState -> Maybe Event -> IO GameState
gameUpdate state Nothing =
    case state of
        GameState snake dir foods (height, width) score -> do
            let foodAte = filter (\(pos, _) -> pos == head snake) foods
                foodLeft = filter (\(pos, _) -> pos /= head snake) foods
                snake' = [head snake `vAdd` dir]
                         ++ if foodAte == [] then init snake else snake
                collided =
                    any (\part ->
                        length (filter (== part) snake') > 1
                        ||
                        let (Pos y x) = part
                        in y < 0 || x < 0
                        ||
                        x >= width
                        ||
                        y >= height
                    ) snake'
                score' = score + toInteger (10 * length foodAte)
            shouldMakeNewFood <- randomRIO (0, length foodLeft * 10) :: IO Int
            newFoods <- if shouldMakeNewFood == 0
                            then makeNewFoods state
                            else return []

            if collided
                then return $ Dead score'
                else return $ GameState snake' dir (foodLeft ++ newFoods) (height, width) score'
        Dead x -> return $ Dead x

gameUpdate (GameState snake dir f w s) (Just ev) = do
    let dir' = case ev of
                EvKey KUp [] ->    Pos (-1) 0
                EvKey KRight [] -> Pos 0 1
                EvKey KDown [] ->  Pos 1 0
                EvKey KLeft [] ->  Pos 0 (-1)
                _ -> dir
    gameUpdate (GameState snake 
                    ( if dir' `sMul` (-1) == dir -- Going backwards
                        then dir
                        else dir'
                    )
                    f w s) Nothing

gameUpdate (Dead score) (Just (EvKey (KChar 'a') [])) = return gameStart
gameUpdate x _ = return x

makeNewFoods :: GameState -> IO [Food]
makeNewFoods state =
    case state of
        (GameState snake _ foods (height, width) _) -> do
            x <- randomRIO (0, width - 1)
            y <- randomRIO (0, height - 1)
            let pos = Pos y x
                collisions = (length $ filter ((== pos) . fst) foods)
                           + (length $ filter (== pos) snake)
            if collisions == 0
                then do 
                    foodType <- (enumFrom Apple !!) <$> randomRIO (0, length (enumFrom Apple) - 1)
                    return [(pos, foodType)]
                else makeNewFoods state
        _ -> return [(Pos 0 0, Apple)]
