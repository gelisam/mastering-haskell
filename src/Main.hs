module Main where

source :: IO [Int]
source = go 1
  where
    go 4 = return []
    go x = do putStrLn $ "source " ++ show x
              xs <- go (x+1)
              return (x:xs)

odds :: [Int] -> IO [Int]
odds []     = return []
odds (x:xs) = do putStrLn $ "filter " ++ show x
                 xs' <- odds xs
                 return $ if odd x then x:xs' else xs'

sink :: [Int] -> IO Int
sink []     = return 0
sink (x:xs) = do putStrLn $ "sink " ++ show x
                 (x +) <$> sink xs

main :: IO ()
main = do r <- source >>= odds >>= sink
          print r
