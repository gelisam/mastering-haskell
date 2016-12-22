module Main where
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Hashable
import System.Random
import Text.Printf

-- ghc src/Main.hs  -threaded -rtsopts -with-rtsopts=-N1
mineCoinsFaster :: MVar Block -> IO ()
mineCoinsFaster var = do
  replicateM_ 4 $ do
    rng <- newRng
    forkIO $ mineCoins var rng

mineCoins :: MVar Block -> Rng -> IO ()
mineCoins var rng = do
  prevBlock@(_, _, blockNumber) <- readMVar var
  when (blockNumber < 25) $ do
    (r, rng') <- tryNextNonce prevBlock rng
    case r of Just block -> do
                prevBlock' <- takeMVar var
                if prevBlock' == prevBlock
                then do printf "%064b\n" (hash block)
                        putMVar var block
                else putMVar var prevBlock
                mineCoins var rng'
              Nothing -> do
                mineCoins var rng'



type Block = (Hash, Nonce, BlockNumber)

tryNextNonce :: Block -> Rng -> IO (Maybe Block, Rng)
tryNextNonce prevBlock@(_,_,blockNumber) rng = do
  let (nonce, rng') = random rng
  let block = (hash prevBlock, nonce, blockNumber+1)
  if countLeadingZeros (hash block) >= 18
  then return (Just block, rng')
  else return (Nothing,    rng')



main :: IO ()
main = do
  var <- newMVar (0, 0, 0)
  mineCoinsFaster var
  
  let loop = do
        threadDelay (10 * 1000)
        (_, _, blockNumber) <- readMVar var
        when (blockNumber < 25) loop
  loop


type Hash        = Int
type Nonce       = Int
type BlockNumber = Int


type Rng = StdGen

newRng :: IO Rng
newRng = mkStdGen <$> randomIO
