#!/usr/bin/env stack
-- stack --resolver lts-14.21 script
{- Use this line instead of the previous one to plot:
- stack --resolver lts-14.21 script --package containers,easyplot,random
-}

import Control.Monad (join, replicateM)
import Data.List (minimumBy)
import qualified Data.Map.Strict as M
import System.Random (randomRIO)
import Text.Printf (printf)
{- Uncomment to plot.
import Graphics.EasyPlot -}

type Seconds = Double
type Probability = Double
type Average = Double
type Minimum = Double
type Maximum = Double
type CustomerType = String
type Customer = (CustomerType, (Average, Maximum))

cdf :: Seconds -> Probability
cdf t = 1.0 - (exp 1 ** (-(t/100)))

inverseCDF :: Probability -> Seconds
inverseCDF t = (-100) * log (1-t)

sampleCustomers :: Int -> IO [(Seconds, Probability)]
sampleCustomers n = map (\y -> (inverseCDF y, y)) <$> replicateM n (randomRIO (0.0,1.0))

rho = 200
bd :: Double -> Double -> Probability -> Seconds
bd alpha beta x = rho * x ** (alpha - 1) * (1 - x) ** (beta - 1)

yellow :: Probability -> Seconds
yellow = bd 2 5

red :: Probability -> Seconds
red = bd 2 2

blue :: Probability -> Seconds
blue = bd 5 1

avgAndMin :: [Seconds] -> (Average, Minimum)
avgAndMin times = ( sum times / fromIntegral (length times)
                  , minimum times )

avgAndMax :: [Seconds] -> (Average, Maximum)
avgAndMax times = ( sum times / fromIntegral (length times)
                  , maximum times )

compare' :: Customer -> Customer -> Ordering
compare' (_, (avg1, max1)) (_, (avg2, max2)) = compare (max1-avg1) (max2-avg2)

main :: IO ()
main = do
  -- Customers Probability and Beta Distribution
  customerModel <- sampleCustomers 1000
{-   Uncomment to plot Customers Probability.
  plot (PNG "CustomerSampling.png") $ Data2D [Style Dots, Title "customer"] [] customerModel -}
  let probs = snd <$> customerModel
  let times = (<$> probs) <$> [yellow, red, blue]
{- Uncomment to plot Beta Distribution.
  plot (PNG "BetaDistribution.png")
       [ Data2D [Title "", Style Dots, Color DarkYellow] [] (zip probs (times !! 0))
       , Data2D [Title "", Style Dots, Color Red] [] (zip probs (times !! 1))
       , Data2D [Title "", Style Dots, Color Blue] [] (zip probs (times !! 2))] -}
  let bDist = M.fromList $ zip ["yellow", "red", "blue"] (avgAndMax <$> times)

  -- Given only yellow customers, what are the average and maximum customer waiting times?
  let yellowAvgAndMax = bDist M.! "yellow"
  putStrLn "Yellow customers waiting times:"
  putStrLn $ printf "Average: %.2f\nMaximum: %.2f\n"
                    (fst yellowAvgAndMax) (snd yellowAvgAndMax)

  -- Given only red customers, what are the average and maximum queue lengths in-front of the teller?
  putStrLn "Red customers queue lengths:"
  let avgAndMinSample = avgAndMin (fst <$> customerModel)
  let avgRedQueue = fst (bDist M.! "red") / fst avgAndMinSample
  putStrLn $ printf "Average: %.2f" avgRedQueue
  let maxRedQueue = snd (bDist M.! "red") / snd avgAndMinSample
  putStrLn $ printf "Maximum: %.2f\n" maxRedQueue

  -- Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?
  putStrLn $ printf "Closest value between the average and maximum customer waiting times: %s customer."
                    (fst $ minimumBy compare' $ M.toList bDist)
