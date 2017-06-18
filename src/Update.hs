module Update
    ( gameUpdate
    , makeNewFoods
    ) where

import Graphics.Vty
import System.Random

import Base

gameUpdate :: GameState -> Maybe Event -> GameState
gameUpdate (Playing state settings) Nothing =
    let (height, width) = getSize settings
        dyingFood = map (\food -> food {getTimeLeft = getTimeLeft food - getDyingSpeed food}) $ getFood state
        foodAte = filter (\food -> getPos food == head snake) dyingFood
        foodLeft = filter (\food -> getPos food /= head snake && getTimeLeft food > 0) dyingFood
        snake = [head (getSnake state) `vAdd` getDirection state]
                 ++ if foodAte == [] then init $ getSnake state else getSnake state
        collided =
            any (\part ->
                length (filter (== part) $ getSnake state) > 1 && length (getSnake state) > 3
                ||
                let (Pos y x) = part
                in y < 0 || x < 0
                ||
                x >= width
                ||
                y >= height
            ) $ getSnake state
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

gameUpdate (Dead score settings) (Just (EvKey (KChar 'a') [])) =
    case getStartState settings of
        Just startState -> Start startState
        Nothing -> Start defaultStartState

gameUpdate (Start state) (Just (EvKey KUp [])) = Start $ state { getCurrentSetting = (getCurrentSetting state - 1) `mod` length (getSettings state) }
gameUpdate (Start state) (Just (EvKey KDown [])) = Start $ state { getCurrentSetting = (getCurrentSetting state + 1) `mod` length (getSettings state) }
gameUpdate (Start state) (Just k) =
    let sIdx = getCurrentSetting state
        settings = getSettings state
        button = getSettings state !! sIdx
    in case button of
        StartGameButton ->
            case k of
                EvKey KEnter [] -> Playing defaultGame ((makeSettings defaultSettings settings) { getStartState = Just state })
                _ -> Start state
        _ -> Start $ state
                { getSettings = 
                       take sIdx settings 
                    ++ [ button { getValue = (sUpdate button) button k } ]
                    ++ drop (sIdx + 1) settings }

gameUpdate x _ = x

makeNewFoods :: GameState -> IO GameState
makeNewFoods (Playing state settings) = do
    shouldMakeNewFood <- fmap (< 1) (randomRIO (0, length (getFood state) * (getDiff settings + 10)))
    if shouldMakeNewFood
        then do
            let (height, width) = getSize settings
            x <- randomRIO (0, width - 1)
            y <- randomRIO (0, height - 1)
            let pos = Pos y x
                collisions = (length $ filter ((== pos) . getPos) $ getFood state)
                           + (length $ filter (== pos) $ getSnake state)
            if collisions == 0
                then do
                    foodType <- (enumFrom Apple !!) <$> randomRIO (0, length (enumFrom Apple) - 1)
                    dyingSpeed <- randomRIO (0, (0.3 + (fromIntegral $ getDiff settings) * 0.02)) :: IO Float
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
        else return $ Playing state settings
makeNewFoods x = return x
