{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Main where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Primitive (Vector, replicate, replicateM, sum, zipWith)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random.MWC (Gen, create, uniformR)
import Text.Read (readMaybe)

import GHC.Exts
import Prelude hiding (replicate, sum, zipWith)

sq :: Double# -> Double#
sq !x = x *## x
{-# INLINE sq #-}

normSquared :: Vector Double -> Double#
normSquared !xs =
  let !(D# x) = sum (zipWith (*) xs xs) in
      x
{-# INLINABLE normSquared #-}

data World = World
  {  walker :: !(Vector Double),
     cachedTrial :: !Double#,
     moves :: !Int#,
     attempts :: !Int#,
     displacement :: !Double#,
     expectedEnergy :: !Double#,
     expectedEnergySquared :: !Double#}

data WorldSpec = WorldSpec
  {  dimension :: Int#,
     trialWaveFunction :: Vector Double -> Double#,
     trialLocalEnergy :: Vector Double -> Double#,
     isFinished :: World -> Int#}

expectedEnergyError :: World -> Double#
expectedEnergyError !World {attempts = j, expectedEnergySquared = eE2} =
  let  !sigma2 = eE2 /## int2Double# (j -# 1#) in
       sqrtDouble# (sigma2 /## int2Double# j)

vmc :: PrimMonad m => Gen (PrimState m) -> WorldSpec -> World -> m World
vmc g WorldSpec
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
         do  !x <- replicateM (I# d) $ uniformR (-1, 1) g
             let  !rP = zipWith (\ !(D# r) !(D# x) -> D# (r +## x *## dr)) r x
                  !psiP = psiT rP
                  !c = sq (psiP /## psi)
             !move <-
               if  tagToEnum# (c >## 1.0##) then
                   return True else
                   do  !(D# p) <- uniformR (0, 1) g
                       return (tagToEnum# (c >## p))
             let  !(# r', psi', i' #)
                    | move = (# rP, psiP, i +# 1# #)
                    | otherwise = (# r, psi, i #)
                  !j' = j +# 1#
                  !dr' = dr *## (int2Double# i' /## int2Double# j' +## 0.5##)
                  !e' = eT r'
                  !de = e' -## eE
                  !eE' = eE +## de /## int2Double# j'
                  !de' = e' -## eE'
                  !eE2' = eE2 +## de *## de'
                  !w' = World
                    {  walker = r',
                       cachedTrial = psi',
                       moves = i',
                       attempts = j',
                       displacement = dr',
                       expectedEnergy = eE',
                       expectedEnergySquared = eE2'}
             if  tagToEnum# (f w') then
                 return w' else
                 vmc' w' in
       vmc'

harmonicPotential :: Vector Double -> Double#
harmonicPotential !r = 0.5## *## normSquared r

waveFunction :: Double# -> Vector Double -> Double#
waveFunction alpha !r = expDouble# (negateDouble# (alpha *## normSquared r))

localEnergy ::
  Double# -> (Vector Double -> Double#) -> Int# -> Vector Double -> Double#
localEnergy alpha v d !r =
  v r -## alpha *## (2.0## *## alpha *## normSquared r -## int2Double# d)

main :: IO ()
main =
  do  as <- getArgs
      case  sequence $ readMaybe <$> as of
            Just [(I# n), (I# d)] | tagToEnum# (andI# (n >=# 0#) (d >=# 0#)) ->
              let  alpha = 2.0## /## 3.0##
                   ws = WorldSpec
                     {  dimension = d,
                        trialWaveFunction = waveFunction alpha,
                        trialLocalEnergy = localEnergy alpha harmonicPotential d,
                        isFinished = \ !World {attempts = j} -> j ==# n}
                   w = World
                     {  walker = replicate (I# d) 0,
                        cachedTrial = 0.0##,
                        moves = 0#,
                        attempts = 0#,
                        displacement = 1.0##,
                        expectedEnergy = 0.0##,
                        expectedEnergySquared = 0.0##} in
                   do  g <- create
                       w' <- vmc g ws w
                       putStrLn $
                         show (D# (expectedEnergy w')) ++ " \xb1 " ++
                         show (D# (expectedEnergyError w'))
            _ -> die "Invalid argument list."
