{-# LANGUAGE Arrows          #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent
import           Data.IORef
import           Data.Time.Clock
import           FRP.Yampa

import           Control.Lens           (makeLenses, set, view)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict    as Map
import           Data.Map.Strict        (Map)
import           Flow
import           Numeric.Natural

data State = State {
    _timestep :: Int,
    _value    :: Float
} deriving (Show)
makeLenses ''State

type StateHistory = [State]

initialState :: State
initialState = State 0 1

stateHistory :: [State]
stateHistory = [initialState]

updateTimestep :: State -> State
updateTimestep state = set timestep ((view timestep state) + 1) state

u1 :: State -> State
u1 state = set value ((view value state) + 1) state

model :: SF State State
model = arr updateTimestep
        >>^ u1

simulation :: State -> SF () State
simulation state = arr (\_ -> state) >>> model

run :: IO a                                 -- init
           -> (Bool -> IO (DTime, Maybe a)) -- input/sense
           -> (Bool -> b -> IO Bool)        -- output/actuate
           -> SF a b                        -- process/signal function
           -> IO ()
run a b c d = reactimate a b c d

timestepDelay :: IO ()
timestepDelay = threadDelay 100000

initialize :: IO ()
initialize = putStrLn "funCAD starting up..." >> threadDelay 1000000

input :: IORef UTCTime -> Bool -> IO (Double, Maybe ())
input timeRef _ = do
    now      <- getCurrentTime
    lastTime <- readIORef timeRef
    writeIORef timeRef now
    -- let dt = now `diffUTCTime` lastTime
    let dt = 0.1
    timestepDelay
    return (realToFrac dt, Just ()) -- return (dt, Nothing)

actuate = (\_ b -> (putStrLn $ show b) >> return False)

main :: IO ()
main = do
    t <- getCurrentTime
    timeRef <- newIORef t
    run initialize (input timeRef) actuate (simulation $ initialState)

-- timesteps = [0.1..10]

-- main :: IO ()
-- main = do
--     mapM_ print (embed (simulation $ initialState) ((), [(dt, Nothing) | dt <- timesteps]))
