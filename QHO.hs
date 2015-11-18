module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

exactEnergy :: Fractional a => Int -> a
exactEnergy d = fromIntegral d / 2

exactEnergyError :: Num a => a
exactEnergyError = 0

main :: IO ()
main =
  do  as <- getArgs
      case  sequence $ readMaybe <$> as of
            Just [n, d] | n >= 0 && d >= 0 ->
              putStrLn $
              show (exactEnergy d :: Double) ++ " \xb1 " ++
              show (exactEnergyError :: Double)
            _ -> die "Invalid argument list."
