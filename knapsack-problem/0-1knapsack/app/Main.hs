module Main where

import Data.Array (listArray,Array, array, (!))
import Data.Array.IO.Internals (IOArray)
import Data.Array.MArray (readArray, writeArray, newArray, freeze)
import Control.Monad (when, forM_)
import Debug.Trace (trace)

main :: IO ()
main = do
    [n,w] <- map read . words <$> getLine
    ws <- listArray (0,n) . (0:) . map read . words <$> getLine
    vs <- listArray (0,n) . (0:) . map read . words <$> getLine
    -- print $ solution2 n w ws vs
    print $ solution1 n w ws vs
    -- solution2 n w ws vs >>= print
    solution3 n w ws vs >>= print
    solution4 n w ws vs >>= print


solution1 :: Int -> Int -> Array Int Int -> Array Int Int -> Int
solution1 n w ws vs = dp ! (n,w)
  where dp = array ((0,0), (n,w)) [((i,j), transfer i j) | i <- [0..n], j <- [0..w]]
        transfer 0 _ = 0
        transfer _ 0 = 0
        transfer i j = max (dp ! (i-1,j)) (if j > ws ! i then dp ! (i-1,j-ws ! i) + vs ! i else 0)

-- solution2 :: Int -> Int -> Array Int Int -> Array Int Int -> Int
-- solution2 n w ws vs = dp ! w
--   where dp = array (0,w) $ [(i,0) | i <- [0..w]] ++ [(j, transfer i j) | i <- [1..n], j <- [w,w-1..ws ! i]]
--         transfer i j = max (dp ! j) (dp ! (j - ws ! i) + vs ! i)

solution2 :: Int -> Int -> Array Int Int -> Array Int Int -> IO Int
solution2 n w ws vs = do
    dp <- newArray (0,w) 0 :: IO (IOArray Int Int)
    let solve :: Int -> Int -> IO ()
        solve i j = do
            let j' = j - ws ! i
            when (j' >= 0 && i >= 1) $ solve (i-1) j'
            when (i >= 1) $ solve (i-1) j
            v1 <- readArray dp j
            when (j' >= 0) $ do
                v2 <- readArray dp j'
                trace (show i ++ ", " ++ show j ++ ": " ++ show (max v1 (v2 + vs ! i)))writeArray dp j $ max v1 (v2 + vs ! i)
    solve n w
    readArray dp w


solution3 :: Int -> Int -> Array Int Int -> Array Int Int -> IO Int
solution3 n w ws vs = do
    dp <- newArray ((0,0), (n,w)) 0 :: IO (IOArray (Int, Int) Int)
    forM_ [1..n] $ \i -> do
        forM_ [1..w] $ \j -> do
            v1 <- readArray dp (i-1,j)
            if j > ws ! i
              then do 
                v2 <- readArray dp (i-1,j-ws ! i)
                writeArray dp (i,j) $ max v1 (v2 + vs ! i)
              else writeArray dp (i,j) v1
    readArray dp (n,w)

solution4 :: Int -> Int -> Array Int Int -> Array Int Int -> IO Int
solution4 n w ws vs = do
    dp <- newArray (0, w) 0 :: IO (IOArray Int Int)
    forM_ [1..n] $ \i -> do
        forM_ [w,w-1..ws!i] $ \j -> do
            v1 <- readArray dp j
            if j > ws ! i
              then do 
                v2 <- readArray dp (j-ws ! i)
                writeArray dp j $ max v1 (v2 + vs ! i)
              else writeArray dp j v1
    readArray dp w