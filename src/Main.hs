module Main where

import           Streamly
import qualified Streamly.Prelude as S
import           Types

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

main :: IO ()
main = do
    s <- S.sum $ asyncly $ do
        -- Each square is performed concurrently, (<>) is concurrent
        x2 <- foldMap (\x -> return $ x * x) [1..100]
        y2 <- foldMap (\y -> return $ y * y) [1..100]
        -- Each addition is performed concurrently, monadic bind is concurrent
        return $ sqrt (x2 + y2)
    print s
