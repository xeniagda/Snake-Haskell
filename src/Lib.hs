module Lib
    ( start
    , Pos(..)
    , GameState(..)
    ) where

import Graphics.Vty

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Base
import Render
import Update

delay :: (RealFrac a) => a -> IO ()
delay = threadDelay . floor . (*1000000)

start = do
    vty <- mkVty =<< standardIOConfig

    events <- atomically $ newTVar ([] :: [Event])
    forkIO $ readEvents events vty

    let loop game = do
            update vty $ render game
            let sleep =
                    case game of
                        Playing state settings -> (0.9 - (fromIntegral $ getDiff settings) / 30) ** (fromInteger (getScore state) / 100 + 1) / 10
                        _ -> 0.01
            delay sleep
            event <- popTVar events

            case event of
                Just (EvKey KEsc []) -> shutdown vty
                Just (EvKey (KChar 'c') [MCtrl]) -> shutdown vty
                Just (EvKey (KChar 'q') []) -> shutdown vty
                _ -> do
                    updated <- makeNewFoods $ gameUpdate game event
                    loop updated
    loop $ Start defaultStartState

-- Event handling

readEvents var vty =
    let loop = do
            event <- nextEvent vty
            pushTVar var event
            loop
    in loop

pushTVar :: TVar [a] -> a -> IO ()
pushTVar var a = atomically
    (modifyTVar var (\l -> l ++ [a]))

popTVar :: TVar [a] -> IO (Maybe a)
popTVar var = atomically
    (
        do
            x <- readTVar var
            modifyTVar var (\l ->
                    case l of
                        (_:x) -> x
                        [] -> []
                )
            case x of
                (a:_) -> return $ Just a
                [] -> return $ Nothing
    )
