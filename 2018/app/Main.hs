module Main where

import DayOne
import DayTwo
import DayThree
import DayFour
import DayFive
import DaySix
import DaySeven
import DayEight
import DayNine
import DayTen
import DayEleven
import DayTwelve
import DayThirteen
import DayFourteen

main :: IO ()
main = do

  let solutions =
        [ dayOneMain
        , dayTwoMain
        , dayThreeMain
        , dayFourMain
        , dayFiveMain
        , daySixMain
        , daySevenMain
        , dayEightMain
        , dayNineMain
        , dayTenMain
        , dayElevenMain
        , dayTwelveMain
        , dayThirteenMain
        , dayFourteenMain
        ]
  let cur = length solutions
  putStrLn ""
  putStrLn $ "Day " ++ show cur ++ ":"
  solutions !! (cur - 1)
