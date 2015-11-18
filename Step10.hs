{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

module Main where

import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random.MWC (Gen, create, uniformR)
import Text.Read (readMaybe)

#ifdef __AVX2__

import GHC.Exts

#else

import GHC.Exts hiding
  (  DoubleX4#, broadcastDoubleX4#, packDoubleX4#, plusDoubleX4#,
     timesDoubleX4#, unpackDoubleX4#)

type DoubleX4# = (# Double#, Double#, Double#, Double# #)

broadcastDoubleX4# :: Double# -> DoubleX4#
broadcastDoubleX4# x = (# x, x, x, x #)

packDoubleX4# :: (# Double#, Double#, Double#, Double# #) -> DoubleX4#
packDoubleX4# x = x

plusDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#
plusDoubleX4#
  (# x0, x1, x2, x3 #)
  (# y0, y1, y2, y3 #) =
  (# x0 +## y0, x1 +## y1, x2 +## y2, x3 +## y3 #)

timesDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#
timesDoubleX4#
  (# x0, x1, x2, x3 #)
  (# y0, y1, y2, y3 #) =
  (# x0 *## y0, x1 *## y1, x2 *## y2, x3 *## y3 #)

unpackDoubleX4# :: DoubleX4# -> (# Double#, Double#, Double#, Double# #)
unpackDoubleX4# x = x

#endif

sq :: Double# -> Double#
sq !x = x *## x
{-# INLINE sq #-}

normSquared :: DoubleX4# -> Double#
normSquared !xs =
  let  sumDoubleX4# !x =
         let  (# x0, x1, x2, x3 #) = unpackDoubleX4# x in
              x0 +## x1 +## x2 +## x3 in
       sumDoubleX4# (timesDoubleX4# xs xs)
{-# INLINABLE normSquared #-}

data World = World
  {  walker :: !DoubleX4#,
     cachedTrial :: !Double#,
     moves :: !Int#,
     attempts :: !Int#,
     displacement :: !Double#,
     expectedEnergy :: !Double#,
     expectedEnergySquared :: !Double#}

data WorldSpec = WorldSpec
  {  dimension :: Int#,
     trialWaveFunction :: DoubleX4# -> Double#,
     trialLocalEnergy :: DoubleX4# -> Double#,
     isFinished :: World -> Int#}

expectedEnergyError :: World -> Double#
expectedEnergyError !World {attempts = j, expectedEnergySquared = eE2} =
  let  sigma2 = eE2 /## int2Double# (j -# 1#) in
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
         do  !(D# x0, D# x1, D# x2, D# x3) <-
               let  !u = uniformR (-1, 1) g in
                    case  d of
                          4# ->
                            do  !x0 <- u
                                !x1 <- u
                                !x2 <- u
                                !x3 <- u
                                return (x0, x1, x2, x3)
                          3# ->
                            do  !x0 <- u
                                !x1 <- u
                                !x2 <- u
                                return (x0, x1, x2, 0)
                          2# ->
                            do  !x0 <- u
                                !x1 <- u
                                return (x0, x1, 0, 0)
                          1# ->
                            do  !x0 <- u
                                return (x0, 0, 0, 0)
                          0# -> return (0, 0, 0, 0)
                          _ -> error "dimension too large"
             let  !x = packDoubleX4# (# x0, x1, x2, x3 #)
                  !rP =
                    plusDoubleX4# r
                    (timesDoubleX4# x (broadcastDoubleX4# dr))
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

harmonicPotential :: DoubleX4# -> Double#
harmonicPotential !r = 0.5## *## normSquared r

waveFunction :: Double# -> DoubleX4# -> Double#
waveFunction alpha !r = expDouble# (negateDouble# (alpha *## normSquared r))

localEnergy :: Double# -> (DoubleX4# -> Double#) -> Int# -> DoubleX4# -> Double#
localEnergy alpha v d !r =
  v r -## alpha *## (2.0## *## alpha *## normSquared r -## int2Double# d)

main :: IO ()
main =
  do  as <- getArgs
      case  sequence $ readMaybe <$> as of
            Just [(I# n), (I# d)]
              | tagToEnum# (andI# (n >=# 0#) (andI# (d >=# 0#) (d <=# 4#))) ->
              let  alpha = 2.0## /## 3.0##
                   ws = WorldSpec
                     {  dimension = d,
                        trialWaveFunction = waveFunction alpha,
                        trialLocalEnergy = localEnergy alpha harmonicPotential d,
                        isFinished = \ !World {attempts = j} -> j ==# n}
                   w = World
                     {  walker = broadcastDoubleX4# 0.0##,
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
