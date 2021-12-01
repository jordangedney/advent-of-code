module X1 (main) where

x :: Set Int 
x = [0]

main :: IO ()
main = x |> show |> putStrLn
