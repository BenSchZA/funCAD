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

updateTimestep :: SF State State
updateTimestep =
    increment &&& id
    ^>> merge
    where
        increment = (+ 1) . (view timestep)
        merge = arr (uncurry (set timestep))

u1 :: SF State State
u1 =
    update &&& id
    ^>> merge
    where
        update = (+ 1) . (view value)
        merge = arr (uncurry (set value))

model :: SF State State
model = u1

simulation :: SF State State
simulation = updateTimestep >>> model

run :: IO a                                 -- init
           -> (Bool -> IO (DTime, Maybe a)) -- input/sense
           -> (Bool -> b -> IO Bool)        -- output/actuate
           -> SF a b                        -- process/signal function
           -> IO ()
run a b c d = reactimate a b c d

timestepDelay :: IO ()
timestepDelay = threadDelay 100000

initialize :: IO State
initialize = putStrLn "funCAD starting up..." >> threadDelay 1000000 >> return initialState

input :: IORef UTCTime -> Bool -> IO (Double, Maybe State)
input timeRef _ = do
    now      <- getCurrentTime
    lastTime <- readIORef timeRef
    writeIORef timeRef now
    -- let dt = now `diffUTCTime` lastTime
    let dt = 0.1
    timestepDelay
    return (realToFrac dt, Nothing)

actuate :: p -> State -> IO Bool
actuate = (\_ b -> (putStrLn $ show b) >> return False)

main :: IO ()
main = do
    t <- getCurrentTime
    timeRef <- newIORef t
    result <- run initialize (input timeRef) actuate simulation
    print result

-- timesteps = [0.1..10000]

-- main :: IO ()
-- main = do
--     mapM_ print (embed (simulation) (initialState, [(dt, Nothing) | dt <- timesteps]))
