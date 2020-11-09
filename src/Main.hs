{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import           Types (Policy)

import           Streamly
import qualified Streamly.Prelude as S
import Flow
import Control.Lens (makeLenses, set)
import qualified Data.HashMap.Strict as Map
import Data.Map.Strict (Map)
import Numeric.Natural


-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

timesteps :: Float
timesteps = 1e3

-- params, substep, state_history, previous_state

data Param = Natural | Integer | Float
type Params = Map.HashMap String [Integer]

type Substep = Natural

data State = State { _key :: String, _value :: Float } deriving (Show)
makeLenses ''State

type States = [State]
type StateHistory = [States]

data Signal = Signal { _signal :: String, _signalValue :: Float } deriving (Show)
type Signals = [Signal]

type Policy = Params -> Substep -> StateHistory -> States -> Signals
-- data PolicyReturn = PolicyReturn { _params :: Params, _substep :: Substep, _stateHistory :: StateHistory, _states :: States, _signals :: Signals }

type Update = Params -> Substep -> StateHistory -> States -> Signals -> State

p_1 :: Policy
p_1 params substep stateHistory states = [Signal "sig" $ (_value $ head states) * 2]

u_1 :: Update
u_1 params substep stateHistory states signal = do
    set value 3 $ head states

initialState = [State "a" 1, State "b" 2]
stateHistory = [initialState]

params :: Params
params = Map.fromList [("a", [1]), ("b", [2])]
substep = 1

(&) :: a -> (a -> b) -> b
x & f = f x

process = p_1 params substep stateHistory initialState & u_1 params substep stateHistory initialState

main :: IO ()
main = do
    print process

-- main :: IO ()
-- main = do
--     s <- S.sum $ asyncly $ do
--         -- Each square is performed concurrently, (<>) is concurrent
--         x2 <- foldMap (\x -> return $ p_1 [x]) [1..1e3]
--         y2 <- foldMap (\y -> return $ y * y) [1..1e3]
--         -- Each addition is performed concurrently, monadic bind is concurrent
--         return $ sqrt (x2 + y2)
--     print s
