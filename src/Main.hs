module Main where
import Control.Monad
import Data.Bits
import Data.Hashable
import System.Random
import Text.Printf

type Block = (Hash, Nonce, BlockNumber)

mineCoins :: Block -> Rng -> IO ()
mineCoins prevBlock@(_,_,blockNumber) rng = do
  when (blockNumber < 25) $ do
    (r, rng') <- tryNextNonce prevBlock rng
    case r of Just block -> do printf "%064b\n" (hash block)
                               mineCoins block rng'
              Nothing -> mineCoins prevBlock rng'

tryNextNonce :: Block -> Rng -> IO (Maybe Block, Rng)
tryNextNonce prevBlock@(_,_,blockNumber) rng = do
  let (nonce, rng') = random rng
  let block = (hash prevBlock, nonce, blockNumber+1)
  if countLeadingZeros (hash block) >= 18
  then return (Just block, rng')
  else return (Nothing,    rng')



main :: IO ()
main = do
  let block0 = (0, 0, 0)
  rng <- newRng
  mineCoins block0 rng



type Hash        = Int
type Nonce       = Int
type BlockNumber = Int


type Rng = StdGen

newRng :: IO Rng
newRng = mkStdGen <$> randomIO
