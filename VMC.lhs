\documentclass{article}
\usepackage[a4paper, margin=1.25in]{geometry}
\usepackage{amsmath, hyperref}

%include polycode.fmt
%include greek.fmt

\begin{document}

%format * = "\thickspace "
%format <$> = "\mathbin{<\negthickspace \$ \negthickspace>} "
%format fromIntegral = "\negthickspace "
%format psiT = "\psi_T "
%format eT = "e_T "
%format dr = "\Delta r "
%format dr' = "\Delta r'' "
%format eE = "\langle e \rangle "
%format eE' = "\langle e \rangle'' "
%format eE2 = "\langle e^2 \rangle "
%format eE2' = "\langle e^2 \rangle'' "
%format rP = "r_P "
%format de = "\Delta e "
%format de' = "\Delta e'' "
%format sigma2' = "\sigma''^2 "
%format delta' = "\delta'' "

\section*{Variational Monte Carlo}

Let us build a physical simulation that
solves the ground state energy of a simple quantum mechanical system with
a stochastic numerical method called variational Monte Carlo.

We begin by importing stateful control flow abstractions, basic system calls,
a simple random number generator and a utility for
parsing command line arguments.
\begin{code}
module Main where

import Control.Monad (replicateM)
import Control.Monad.State (State, evalState, state)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random (Random, RandomGen, mkStdGen, randomR)
import Text.Read (readMaybe)
\end{code}

Before getting to the physics, we define a function for squaring a number.
It may seem pointless, but it is a good idea, because
otherwise we would always have to write |x ^ (2 :: Int)| as
mere |x ^ 2| for some |x| leaves the type of |2| ambiguous.
Having a dedicated function is also nicely symmetric with |sqrt|.
\begin{code}
sq :: Num a => a -> a
sq x = x * x
\end{code}
We also define vectors and their norms in Euclidean space.
\begin{code}
data Vector a = Vector [a]

norm :: Floating a => Vector a -> a
norm (Vector xs) = sqrt (sum (sq <$> xs))
\end{code}

Our system consists of one particle in a time-invariant potential well.
We intend to move the particle around randomly with some constraints, so
we affectionately refer to it as a walker making attempts at displacing itself.
The idea is to guess how the walker might act,
formulate a trial wave function based on that,
set the walker on its way, gather statistics of its progress and
measure how good the initial guess actually was.
The better the guess, the smaller the error and
consequently the more useful the result.
We define a data structure to keep track of all this.
\begin{code}
data World a = World
  {  dimension :: Int,
     trialWaveFunction :: Vector a -> a,
     trialLocalEnergy :: Vector a -> a,
     isFinished :: World a -> Bool,
     walker :: Vector a,
     moves :: Int,
     attempts :: Int,
     displacement :: a,
     expectedEnergy :: a,
     expectedEnergySquared :: a,
     expectedEnergyError :: a}
\end{code}

The process itself is defined in terms of an inductive function that
is embedded into the state monad, which
is further specialized for a class of random number generators.%
\footnote{
  This is just a fancy way of saying that have a finite loop that
  can draw random numbers out of thin air.
}
We use this contraption to implement the Metropolis--Hastings algorithm and
throw in a stable online algorithm for
calculating the mean energy and its error.
\begin{code}
vmc ::
  (Floating a, Ord a, Random a, RandomGen g) => World a -> State g (World a)
vmc w @ World
  {  dimension = d,
     trialWaveFunction = psiT,
     trialLocalEnergy = eT,
     isFinished = f,
     walker = Vector r,
     moves = i,
     attempts = j,
     displacement = dr,
     expectedEnergy = eE,
     expectedEnergySquared = eE2} =
  do  x <- replicateM d . state $ randomR (-1, 1)
      let  rP = zipWith (\ r x -> r + x * dr) r x
           c = sq (psiT (Vector rP) / psiT (Vector r))
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
           e' = eT (Vector r')
           de = e' - eE
           eE' = eE + de / fromIntegral j'
           de' = e' - eE'
           eE2' = eE2 + de * de'
           sigma2' = eE2' / fromIntegral j
           delta' = sqrt (sigma2' / fromIntegral j')
           w' = w
             {  walker = Vector r',
                moves = i',
                attempts = j',
                displacement = dr',
                expectedEnergy = eE',
                expectedEnergySquared = eE2',
                expectedEnergyError = delta'}
      if  f w' then
          return w' else
          vmc w'
\end{code}

Since we are going for simplicity,
our test case shall feature the harmonic potential
\begin{align}
  V
  & = \frac 1 2 m \omega^2 r^2.
\end{align}
In operator form
\begin{align}
  \hat V \psi_T
  & = V \psi_T.
\end{align}
We shall also choose such units that $m = \omega = \hbar = 1$, which
makes most constants vanish.
\begin{code}
harmonicPotential :: Floating a => Vector a -> a
harmonicPotential r = (1 / 2) * sq (norm r)
\end{code}

The next step is to guess what kind of
a wave function such a potential might yield.
Intuition dictates that it should be something that
looks like a normal distribution, so let
\begin{align}
  \psi_T
  & = e^{-\alpha r^2}
\end{align}
for some variational parameter $\alpha$.
\begin{code}
waveFunction :: Floating a => a -> Vector a -> a
waveFunction alpha r = exp (-alpha * sq (norm r))
\end{code}
Inserting the potential and our educated guess into
the Schr\"odinger equation gives us the operator form of our Hamiltonian
\begin{align}
  \hat H \psi_T
  & = -\frac{\hbar^2}{2 m} \nabla_r^2 \psi_T + \hat V \psi_T
  = -\frac{\hbar^2}{2 m}
  \frac 1{r^{d - 1}} \partial_r r^{d - 1} \partial_r e^{-\alpha r^2}
  + V e^{-\alpha r^2}
\end{align}
and thus the local energy
\begin{align}
  E_L
  & = \frac{\hat H \psi_T}{\psi_T}
  = -\frac{\hbar^2}{2 m} (4 \alpha^2 r^2 - 2 \alpha d) + V.
\end{align}
\begin{code}
localEnergy :: Floating a => a -> (Vector a -> a) -> Int -> Vector a -> a
localEnergy alpha v d r =
  v r - alpha * (2 * alpha * sq (norm r) - fromIntegral d)
\end{code}

The user shall supply the amount of iterations and the dimension of our system.
Everything else is fixed.
A real simulation would also vary |alpha|, but we fix it here, because
we are lazy and |2 / 3| seems like a pretty good guess.
\begin{code}
main :: IO ()
main =
  do  as <- getArgs
      case  sequence $ readMaybe <$> as of
            Just [n, d] | n >= 0 && d >= 0 ->
              let  alpha = 2 / 3
                   w = World
                     {  dimension = d,
                        trialWaveFunction = waveFunction alpha,
                        trialLocalEnergy = localEnergy alpha harmonicPotential d,
                        isFinished = \ World {attempts = j} -> j == n,
                        walker = Vector (replicate d 0),
                        moves = 0,
                        attempts = 0,
                        displacement = 1,
                        expectedEnergy = 0,
                        expectedEnergySquared = 0,
                        expectedEnergyError = 0}
                   w' = evalState (vmc w) (mkStdGen 0) in
                   putStrLn $
                   show (expectedEnergy w' :: Double) ++ " \xb1 " ++
                   show (expectedEnergyError w' :: Double)
            _ -> die "Invalid argument list."
\end{code}

Alas, our program is really slow and
runs out of memory when the amount of iterations is large enough.
Luckily there is a cure: optimization!

\vspace* \baselineskip \hrule \vspace* \baselineskip
\noindent
\hfill \textsc{Too Much Later} \hfill
\vspace* \baselineskip \hrule \vspace* \baselineskip

In this case the problem can be solved analytically.
For the harmonic oscillator, the ground state wave function
\begin{align}
  \psi_0
  & = \sqrt[4]{\frac{2 m \omega}{2 \pi \hbar}} \: e^{-m \omega r^2 / 2 \hbar}
\end{align}
and the ground state energy
\begin{align}
  E_0
  & = \frac d 2 \hbar \omega.
\end{align}
Even in the general case there are better algorithms than
just variational Monte Carlo alone.

Turns our optimization was pointless after all, because
algorithmic improvements always win.

\end{document}
