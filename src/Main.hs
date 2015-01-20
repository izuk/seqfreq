import Control.Monad (foldM_, replicateM, (>=>))
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (chr)
import Data.Word (Word64)

import Debug.Trace (trace)

type Seed = Word64

data LCG = LCG Word64 Word64 Word64

rand :: Int -> LCG -> Seed -> Seed
rand i (LCG m a c) s = (((s * a) `mod` m) + c + fromIntegral i) `mod` m

type Counts = IOArray Word64 Int

type Table = IOArray Int Counts

empty :: [Seed]
empty = repeat 0

count :: [Seed] -> Table -> IO Int
count seeds tab = do
  rows <- A.getElems tab
  counts <- sequence $ zipWith f rows seeds
  return $ minimum counts
  where
    f row seed = do
      (_, max) <- A.getBounds row
      let i = seed `mod` (max + 1)
      A.readArray row i

increment :: [Seed] -> Table -> IO Int
increment seeds tab = do
  rows <- A.getElems tab
  counts <- sequence $ zipWith f rows seeds
  return $ minimum counts
  where
    f row seed = do
      (_, max) <- A.getBounds row
      let i = seed `mod` (max + 1)
      count <- A.readArray row i
      A.writeArray row i (count + 1)
      return count

roll :: Int -> [LCG] -> [Seed] -> [Seed]
roll ch = zipWith $ rand ch

step :: [LCG] -> Table -> [Seed] -> Int -> IO [Seed]
step lcgs tab seeds ch = do
  let next = roll ch lcgs seeds
  n <- increment next tab
  return $! if n > 2 then next else empty

search :: [LCG] -> Table -> IO [([Int], [Seed], Int)]
search lcgs tab = go [] empty
  where
    go prefix _ | length prefix > 3 = return []
    go prefix seeds = do
      ns <- mapM (\ch -> let next = roll ch lcgs seeds
                         in do n <- count next tab
                               return (ch : prefix, next, n)) [0 .. 255]
      let ns' = filter ((>= 20 ) . thd) ns
      sub <- mapM (\(prefix', next, _) -> go prefix' next) ns'
      return $ ns' ++ concat sub

thd :: (a, b, c) -> c
thd (_, _, x) = x                    

bits = 1000
height = length lcgs
lcgs = [ LCG (2 ^ 32) 1664525 1013904223
       , LCG (2 ^ 32) 22695477 1
       , LCG (2 ^ 31) 1103515245 12345
       ]

main :: IO ()
main = do
  rows <- replicateM height $ A.newArray (0, bits - 1) 0
  tab <- A.newListArray (0, height - 1) rows :: IO Table
  contents <- B.getContents
  foldM_ (step lcgs tab) empty (map fromIntegral $ B.unpack contents)
  found <- search lcgs tab
  mapM_ (\(prefix, _, n) -> putStrLn ((reverse $ map chr prefix) ++ " " ++ show n)) found
  --mapM_ (A.getElems >=> print) rows
