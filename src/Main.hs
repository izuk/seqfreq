{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.DeepSeq (NFData, ($!!))
import Control.Monad ((>=>), foldM, replicateM, zipWithM)
import Data.Array.IO (IOUArray)
import qualified Data.Array.IO as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (chr, ord)
import Data.Word (Word64, Word8)
import Text.Printf (printf)

class Gen g a | g -> a where
  reset :: g -> g
  next :: a -> g -> g
  index :: g -> (Int, Int) -> Int

resetAll :: Gen g a => [g] -> [g]
resetAll = map reset
{-# INLINE resetAll #-}

data LCG = LCG
           { lcgM :: !Word64
           , lcgA :: !Word64
           , lcgC :: !Word64
           , lcgS :: !Word64
           }

instance NFData LCG

instance Show LCG where
  show (LCG m a c s) = printf "%d {%d|%d|%d}" s m a c

instance Gen LCG Word8 where
  reset g@(LCG _ _ c _) = g { lcgS = c }
  next ch g@(LCG m a c s) = g { lcgS = (((s * a) `mod` m) + c + fromIntegral ch) `mod` m }
  index g@(LCG _ _ _ s) (min, max) = (fromIntegral s `mod` (max - min + 1)) + min

type Counts = IOUArray Int Int
type Table = [Counts]

count :: Gen g a => [g] -> Table -> IO Int
count gens tab = do
  counts <- zipWithM f tab gens
  return $! minimum counts
  where
    f row gen = do
      bounds <- A.getBounds row
      let i = index gen bounds
      A.readArray row i

increment :: Gen g a => [g] -> Table -> IO Int
increment gens tab = do
  counts <- zipWithM f tab gens
  return $! minimum counts
  where
    f row gen = do
      bounds <- A.getBounds row
      let i = index gen bounds
      count <- A.readArray row i
      A.writeArray row i $ count + 1
      return count

roll :: Gen g a => a -> [g] -> [g]
roll ch = map (next ch)
{-# INLINE roll #-}

minExtend = 2

step :: (NFData g, Gen g a) => Table -> [g] -> a -> IO [g]
step tab gens ch = do
  let gens' = roll ch gens
  n <- increment gens' tab
  let next = if n >= minExtend
             then gens'
             else resetAll gens'
  return $!! next

maxSize = 50
minCount = 100
choices = [1 .. 127]

search :: Table -> [LCG] -> IO ()
search tab = go []
  where
    go prefix _ | length prefix > maxSize = return ()
    go prefix gens = do
      ns <- mapM (\ch -> let gens' = roll ch gens
                         in do n <- count gens' tab
                               return (ch : prefix, gens', n)) choices
      let ns' = filter ((>= minCount) . thd) ns
      mapM_ (\(prefix', gens', n) -> do
                putStrLn $ show n ++ " " ++ reverse (map (chr . fromIntegral) prefix')
                go prefix' gens') ns'

thd :: (a, b, c) -> c
thd (_, _, x) = x                    

bits = 1000000
height = length lcgs
lcgs = [ LCG (2 ^ 32) 1664525 1013904223 0
       , LCG (2 ^ 32) 22695477 1 0
       , LCG (2 ^ 31) 1103515245 12345 0
       , LCG (2 ^ 32) 134775813 1 0
       , LCG (2 ^ 32) 214013 2531011 0
       , LCG (2 ^ 32) 1140671485 12820163 0
       ]

main :: IO ()
main = do
  tab <- replicateM height $ A.newArray (0, bits - 1) 0 :: IO Table
  let root = resetAll lcgs
  B.getContents >>= foldM (step tab) root . B.unpack
  --mapM_ (A.getElems >=> print) tab
  search tab root

