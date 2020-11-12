{-# LANGUAGE Arrows         #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import           Control.Concurrent
import           FRP.Yampa
-- import           Streamly
-- import qualified Streamly.Prelude   as S

data State = State { value :: Double } deriving (Show)
initialState = State 25.0

cooling :: State -> SF () (State)
cooling state = (constant (-1) >>> integral) >>^ (+ value state) >>^ State

coolingWithFloor :: State -> SF () (State)
coolingWithFloor state = switch cooling' atRoomTemp
    where t' :: SF () State
          t' = cooling state
          branch_a :: SF () State
          branch_a = t'
          branch_b :: SF () (Event (SF () State))
          branch_b = (t' >>> value ^>> (<= 18) ^>> edge >>^ (\e -> e `tag` t'))
          cooling' :: SF () (State, Event (SF () State))
          cooling' = branch_a &&& branch_b
          atRoomTemp :: p -> SF b State
          atRoomTemp = (\_ -> constant 18 >>^ State)

main :: IO ()
main = reactimate (return ())
                (\_ -> threadDelay 100000 >> return (0.1, Nothing))
                (\_ b -> (putStrLn $ show b) >> return False)
                (coolingWithFloor initialState)

-- cooling :: Double -> SF () (Double)
-- cooling t0 = proc input -> do
--                t0' <- integral >>^ (+ t0) -< -1
--                returnA -< t0'

-- coolingWithFloor :: Double -> SF () (Double)
-- coolingWithFloor t0 = switch cooling' atRoomTemp
--     where cooling' = proc _ -> do
--                         t' <- cooling t0 -< ()
--                         e <- edge -< t' <= 18
--                         returnA -< (t', e `tag` t')
--           atRoomTemp _ = (constant 18)

-- main :: IO ()
-- main = reactimate (return ())
--                 (\_ -> threadDelay 100000 >> return (0.1, Nothing))
--                 (\_ b -> (putStrLn $ show b) >> return False)
--                 (coolingWithFloor 25.0)

-- main :: IO ()
-- main = do
--     s <- S.sum $ asyncly $ do
--         -- Each square is performed concurrently, (<>) is concurrent
--         x2 <- foldMap (\x -> return $ p_1 [x]) [1..1e3]
--         y2 <- foldMap (\y -> return $ y * y) [1..1e3]
--         -- Each addition is performed concurrently, monadic bind is concurrent
--         return $ sqrt (x2 + y2)
--     print s


-- data Param = Natural | Integer | Float
-- type Params = Map.HashMap String [Integer]

-- type Substep = Natural

-- data Signal = Signal { _signal :: String, _signalValue :: Float } deriving (Show)
-- type Signals = [Signal]

-- type Policy = Params -> Substep -> StateHistory -> States -> Signals
-- -- data PolicyReturn = PolicyReturn { _params :: Params, _substep :: Substep, _stateHistory :: StateHistory, _states :: States, _signals :: Signals }

-- type Update = Params -> Substep -> StateHistory -> States -> Signals -> State

-- p_1 :: Policy
-- p_1 _params _substep _stateHistory states = [Signal "sig" $ (_value $ head states) * 2]

-- u_1 :: Update
-- u_1 _params _substep _stateHistory states _signal = do
--     set value 3 $ head states

-- params :: Params
-- params = Map.fromList [("a", [1]), ("b", [2])]
-- substep :: Substep
-- substep = 1

-- -- (&) :: a -> (a -> b) -> b
-- -- x & f = f x

-- -- process :: State
-- -- process = p_1 params substep stateHistory initialState & u_1 params substep stateHistory initialState
