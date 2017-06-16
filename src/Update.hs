module Update
    ( gameUpdate
    , makeNewFoods
    ) where

import Graphics.Vty
import System.Random

import Base

gameUpdate :: GameState -> Maybe Event -> GameState
gameUpdate (Playing state settings) Nothing =
    let (height, width) = getSize state
        dyingFood = map (\food -> food {getTimeLeft = getTimeLeft food - getDyingSpeed food}) $ getFood state
        foodAte = filter (\food -> getPos food == head snake) dyingFood
        foodLeft = filter (\food -> getPos food /= head snake && getTimeLeft food > 0) dyingFood
        snake = [head (getSnake state) `vAdd` getDirection state]
                 ++ if foodAte == [] then init $ getSnake state else getSnake state
        collided =
            any (\part ->
                length (filter (== part) snake) > 1
                ||
                let (Pos y x) = part
                in y < 0 || x < 0
                ||
                x >= width
                ||
                y >= height
            ) snake
        score = getScore state + toInteger (10 * length foodAte)
    in if collided
        then Dead score settings
        else Playing 
                (state { getSnake = snake, getFood = foodLeft, getScore = score })
                settings

gameUpdate (Playing state settings) (Just ev) =
    let dir = case ev of
                EvKey KUp [] ->    Pos (-1) 0
                EvKey KRight [] -> Pos 0 1
                EvKey KDown [] ->  Pos 1 0
                EvKey KLeft [] ->  Pos 0 (-1)
                _ -> getDirection state
    in gameUpdate 
        ( Playing (state 
            { getDirection =
                ( if dir `sMul` (-1) == getDirection state -- Going backwards
                      then getDirection state
                      else dir
                )
            })
            settings
        )
        Nothing

gameUpdate (Dead score settings) (Just (EvKey (KChar 'a') [])) = gameStart
gameUpdate x _ = x


makeNewFoods :: GameState -> IO GameState
makeNewFoods (Playing state settings) = do
    shouldMakeNewFood <- randomRIO (0, length (getFood state) * 10) :: IO Int
    if shouldMakeNewFood > 1
        then return $ Playing state settings
        else do
            let (height, width) = getSize state
            x <- randomRIO (0, width - 1)
            y <- randomRIO (0, height - 1)
            let pos = Pos y x
                collisions = (length $ filter ((== pos) . getPos) $ getFood state)
                           + (length $ filter (== pos) $ getSnake state)
            if collisions == 0
                then do 
                    foodType <- (enumFrom Apple !!) <$> randomRIO (0, length (enumFrom Apple) - 1)
                    dyingSpeed <- randomRIO (0, 0.3) :: IO Float
                    return $ 
                        Playing 
                            ( state 
                                { getFood = ((
                                    Food 
                                        {getPos = pos, getType = foodType
                                        , getTimeLeft = 1
                                        , getDyingSpeed = dyingSpeed ^ 3}) 
                                    : getFood state
                                )
                                }
                            )
                            settings
                else makeNewFoods $ Playing state settings
makeNewFoods x = return x
