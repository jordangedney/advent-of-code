import Lude
import Crypto.Hash (Digest, MD5, hash) 
import Data.ByteString.Char8 (pack)

md5 :: String -> String
md5 input = show (pack input & hash :: Digest MD5)

main :: IO ()
main = do
  let input = "iwrupvqb"

  -- part one
  let fiveZeros ('0':'0':'0':'0':'0':_) = True
      fiveZeros _                       = False
      keys = map ((input ++) . show) [0..]
  print $ keys & dropWhile (not . (md5 |> fiveZeros)) & head

  -- part two
  let sixZeros ('0':'0':'0':'0':'0':'0':_) = True
      sixZeros _                           = False
  print $ keys & dropWhile (not . (md5 |> sixZeros)) & head