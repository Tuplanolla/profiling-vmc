module Main where

import Control.Monad (replicateM)
import Control.Monad.State (State, evalState, state)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random (Random, RandomGen, mkStdGen, randomR)
import Text.Read (readMaybe)

sq :: Num a => a -> a
sq x = x * x

type Vector a = [a]

norm :: Floating a => Vector a -> a
norm xs = sqrt (sum (sq <$> xs))

data World a = World
  {  walker :: Vector a,
     moves :: Int,
     attempts :: Int,
     displacement :: a,
     expectedEnergy :: a,
     expectedEnergySquared :: a,
     expectedEnergyError :: a}

data WorldSpec a = WorldSpec
  {  dimension :: Int,
     trialWaveFunction :: Vector a -> a,
     trialLocalEnergy :: Vector a -> a,
     isFinished :: World a -> Bool}

vmc ::
  (Floating a, Ord a, Random a, RandomGen g) =>
  WorldSpec a -> World a -> State g (World a)
vmc ws @ WorldSpec
  {  dimension = d,
     trialWaveFunction = psiT,
     trialLocalEnergy = eT,
     isFinished = f} World
  {  walker = r,
     moves = i,
     attempts = j,
     displacement = dr,
     expectedEnergy = eE,
     expectedEnergySquared = eE2} =
  do  x <- replicateM d . state $ randomR (-1, 1)
      let  rP = zipWith (\ r x -> r + x * dr) r x
           c = sq (psiT rP / psiT r)
      move <-
        if  c > 1 then
            return True else
            do  p <- state $ randomR (0, 1)
                return $ c > p
      let  (r', i')
             | move = (rP, i + 1)
             | otherwise = (r, i)
           j' = j + 1
           dr' = dr * (fromIntegral i' / fromIntegral j' + 1 / 2)
           e' = eT r'
           de = e' - eE
           eE' = eE + de / fromIntegral j'
           de' = e' - eE'
           eE2' = eE2 + de * de'
           sigma2' = eE2' / fromIntegral j
           delta' = sqrt (sigma2' / fromIntegral j')
           w' = World
             {  walker = r',
                moves = i',
                attempts = j',
                displacement = dr',
                expectedEnergy = eE',
                expectedEnergySquared = eE2',
                expectedEnergyError = delta'}
      if  f w' then
          return w' else
          vmc ws w'

harmonicPotential :: Floating a => Vector a -> a
harmonicPotential r = (1 / 2) * sq (norm r)

waveFunction :: Floating a => a -> Vector a -> a
waveFunction alpha r = exp (-alpha * sq (norm r))

localEnergy :: Floating a => a -> (Vector a -> a) -> Int -> Vector a -> a
localEnergy alpha v d r =
  v r - alpha * (2 * alpha * sq (norm r) - fromIntegral d)

main :: IO ()
main =
  do  as <- getArgs
      case  sequence $ readMaybe <$> as of
            Just [n, d] | n >= 0 && d >= 0 ->
              let  alpha = 2 / 3
                   ws = WorldSpec
                     {  dimension = d,
                        trialWaveFunction = waveFunction alpha,
                        trialLocalEnergy = localEnergy alpha harmonicPotential d,
                        isFinished = \ World {attempts = j} -> j == n}
                   w = World
                     {  walker = replicate d 0,
                        moves = 0,
                        attempts = 0,
                        displacement = 1,
                        expectedEnergy = 0,
                        expectedEnergySquared = 0,
                        expectedEnergyError = 0}
                   w' = evalState (vmc ws w) (mkStdGen 0) in
                   putStrLn $
                   show (expectedEnergy w' :: Double) ++ " \xb1 " ++
                   show (expectedEnergyError w' :: Double)
            _ -> die "Invalid argument list."
