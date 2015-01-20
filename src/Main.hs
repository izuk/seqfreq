import Control.Monad (foldM_, replicateM, (>=>))
import Data.Array.IO (IOUArray)
import qualified Data.Array.IO as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (chr, ord)
import Data.Word (Word64)

import Debug.Trace (trace)

type Seed = Word64

data LCG = LCG Word64 Word64 Word64

rand :: Int -> LCG -> Seed -> Seed
rand i (LCG m a c) s = (((s * a) `mod` m) + c + fromIntegral i) `mod` m

type Counts = IOUArray Word64 Int

type Table = [Counts]

empty :: [Seed]
empty = repeat 0

count :: [Seed] -> Table -> IO Int
count seeds tab = do
  counts <- sequence $ zipWith f tab seeds
  return $ minimum counts
  where
    f row seed = do
      (_, max) <- A.getBounds row
      let i = seed `mod` (max + 1)
      A.readArray row i

increment :: [Seed] -> Table -> IO Int
increment seeds tab = do
  counts <- sequence $ zipWith f tab seeds
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

minExtend = 20

step :: [LCG] -> Table -> [Seed] -> Int -> IO [Seed]
step lcgs tab seeds ch = do
  let next = roll ch lcgs seeds
  n <- increment next tab
  return $! if n > minExtend then next else empty

maxSize = 50
minCount = 150
choices = [1 .. 127]
--choices = map ord $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "{},-./= "

search :: [LCG] -> Table -> IO ()
search lcgs tab = go [] empty
  where
    go prefix _ | length prefix > maxSize = return ()
    go prefix seeds = do
      ns <- mapM (\ch -> let next = roll ch lcgs seeds
                         in do n <- count next tab
                               return (ch : prefix, next, n)) choices
      let ns' = filter ((>= minCount) . thd) ns
      mapM_ (\(prefix', next, n) -> do
                putStrLn $ show n ++ " " ++ (reverse $ map chr prefix')
                go prefix' next) ns'

thd :: (a, b, c) -> c
thd (_, _, x) = x                    

bits = 10000
height = length lcgs
lcgs = [ LCG (2 ^ 32) 1664525 1013904223
       , LCG (2 ^ 32) 22695477 1
       , LCG (2 ^ 31) 1103515245 12345
       ]

main :: IO ()
main = do
  tab <- replicateM height $ A.newArray (0, bits - 1) 0 :: IO Table
  contents <- B.getContents
  foldM_ (step lcgs tab) empty (map fromIntegral $ B.unpack contents)
  search lcgs tab
  --mapM_ (A.getElems >=> print) rows
