module Main where
microservice :: A -> B -> IO ()
microservice A1 B1 = undefined
microservice A1 B2 = undefined
microservice A2 B1 = undefined
microservice A2 B2 = undefined

monolith :: A -> B -> C -> D -> IO ()
monolith A1 B1 C1 D1 = undefined
monolith A1 B1 C1 D2 = undefined
monolith A1 B1 C2 D1 = undefined
monolith A1 B1 C2 D2 = undefined
monolith A1 B2 C1 D1 = undefined
monolith A1 B2 C1 D2 = undefined
monolith A1 B2 C2 D1 = undefined
monolith A1 B2 C2 D2 = undefined
monolith A2 B1 C1 D1 = undefined
monolith A2 B1 C1 D2 = undefined
monolith A2 B1 C2 D1 = undefined
monolith A2 B1 C2 D2 = undefined
monolith A2 B2 C1 D1 = undefined
monolith A2 B2 C1 D2 = undefined
monolith A2 B2 C2 D1 = undefined
monolith A2 B2 C2 D2 = undefined



data A = A1 | A2
data B = B1 | B2
data C = C1 | C2
data D = D1 | D2



main :: IO ()
main = return ()
