{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State.Strict (State, evalState, state)
import Data.Vector.Unboxed
  (Unbox, Vector, map, replicate, replicateM, sum, zipWith)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random (Random, RandomGen, mkStdGen, randomR)
import Text.Read (readMaybe)

import Prelude hiding (map, replicate, sum, zipWith)

sq :: Num a => a -> a
sq !x = x * x
{-# INLINE sq #-}

normSquared :: (Num a, Unbox a) => Vector a -> a
normSquared !xs = sum (sq `map` xs)
{-# INLINABLE normSquared #-}

data World a = World
  {  walker :: !(Vector a),
     cachedTrial :: !a,
     moves :: {-# UNPACK #-} !Int,
     attempts :: {-# UNPACK #-} !Int,
     displacement :: !a,
     expectedEnergy :: !a,
     expectedEnergySquared :: !a}

data WorldSpec a = WorldSpec
  {  dimension :: Int,
     trialWaveFunction :: Vector a -> a,
     trialLocalEnergy :: Vector a -> a,
     isFinished :: World a -> Bool}

expectedEnergyError :: Floating a => World a -> a
expectedEnergyError !World {attempts = j, expectedEnergySquared = eE2} =
  let  !sigma2 = eE2 / fromIntegral (j - 1) in
       sqrt (sigma2 / fromIntegral j)

vmc ::
  (Fractional a, Ord a, Random a, RandomGen g, Unbox a) =>
  WorldSpec a -> World a -> State g (World a)
vmc WorldSpec
  {  dimension = d,
     trialWaveFunction = psiT,
     trialLocalEnergy = eT,
     isFinished = f} =
  let  vmc'
         !World
         {  walker = r,
            cachedTrial = psi,
            moves = i,
            attempts = j,
            displacement = dr,
            expectedEnergy = eE,
            expectedEnergySquared = eE2} =
         do  !x <- replicateM d . state $ randomR (-1, 1)
             let  !rP = zipWith (\ !r !x -> r + x * dr) r x
                  !psiP = psiT rP
                  !c = sq (psiP / psi)
             !move <-
               if  c > 1 then
                   return True else
                   do  !p <- state $ randomR (0, 1)
                       return $ c > p
             let  !(r', psi', i')
                    | move = (rP, psiP, i + 1)
                    | otherwise = (r, psi, i)
                  !j' = j + 1
                  !dr' = dr * (fromIntegral i' / fromIntegral j' + 1 / 2)
                  !e' = eT r'
                  !de = e' - eE
                  !eE' = eE + de / fromIntegral j'
                  !de' = e' - eE'
                  !eE2' = eE2 + de * de'
                  !w' = World
                    {  walker = r',
                       cachedTrial = psi',
                       moves = i',
                       attempts = j',
                       displacement = dr',
                       expectedEnergy = eE',
                       expectedEnergySquared = eE2'}
             if  f w' then
                 return w' else
                 vmc' w' in
       vmc'

harmonicPotential :: (Fractional a, Unbox a) => Vector a -> a
harmonicPotential !r = (1 / 2) * normSquared r

waveFunction :: (Floating a, Unbox a) => a -> Vector a -> a
waveFunction alpha !r = exp (-alpha * normSquared r)

localEnergy :: (Num a, Unbox a) => a -> (Vector a -> a) -> Int -> Vector a -> a
localEnergy alpha v d !r =
  v r - alpha * (2 * alpha * normSquared r - fromIntegral d)

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
                        isFinished = \ !World {attempts = j} -> j == n}
                   w = World
                     {  walker = replicate d 0,
                        cachedTrial = 0,
                        moves = 0,
                        attempts = 0,
                        displacement = 1,
                        expectedEnergy = 0,
                        expectedEnergySquared = 0}
                   w' = evalState (vmc ws w) (mkStdGen 0) in
                   putStrLn $
                   show (expectedEnergy w' :: Double) ++ " \xb1 " ++
                   show (expectedEnergyError w' :: Double)
            _ -> die "Invalid argument list."
