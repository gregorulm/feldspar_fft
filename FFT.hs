module Examples.Simple.FFT where

import Prelude hiding (break)

import Core
import Interpretation

import Examples.Simple.Expr
-- These are irrelevant: we want to handle list of signals
-- import Examples.Simple.Filters (eval, comp)

import Frontend.Signal (Sig)
import Frontend.Stream (Str, Stream(..))
import Backend.Compiler.Compiler
import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str
import qualified Backend.C       as B

import Control.Monad
import Control.Monad.Operational (Program)
import Text.PrettyPrint.Mainland
import Data.IORef
import Data.Array.IO.Safe
import qualified System.IO as IO
import qualified Text.Printf as Printf


import Data.Complex

--------------------------------------------------------------------------------
-- * Misc Types
--------------------------------------------------------------------------------

type E = Expr

type S = Sig E

type P = Program (CMD E)


--------------------------------------------------------------------------------
-- * Playing with signals
--------------------------------------------------------------------------------

oneComplex :: Expr (Complex Double)
oneComplex = 1.0

onesSig :: S (Complex Double)
onesSig = S.repeat oneComplex

--------------------------------------------------------------------------------
-- * Test our FFT
--------------------------------------------------------------------------------

-- We use comp and eval from Example.Filters.

-- Test cases with input is of size 8
testFft = comp (fft 8)
evalFft = eval (fft 8)

--------------------------------------------------------------------------------
-- * Evaluating and compiling lists of signals
--------------------------------------------------------------------------------

-- | Evaluating a Signal stream
eval :: ([S (Complex Double)] -> [S (Complex Double)]) -> IO ()
eval = connectIO >=> B.runProgram

-- | ...
comp :: ([S (Complex Double)] -> [S (Complex Double)]) -> IO Doc
comp = connectIO >=> B.cgen . mkFunction "main"

-- | Calling the compiler (compiler not done yet)
connectIO :: ([S (Complex Double)] -> [S (Complex Double)]) -> IO (P ())
connectIO s = undefined 
{-  do 
  prg <- compiler s
  return $ do
    inp  <- open "input"
    outp <- open "output"

    let (Stream init) = prg $ Str.stream $ return $ do
          i     <- fget inp
          isEOF <- feof inp
          iff isEOF break (return ())
            -- Apparently EOF can only be detected after one has tried to read past the end
          return i

    let setty = fput outp
    getty <- init
    while (return $ litExp True)
          (do v <- getty
              setty v)

    close inp
    close outp
-}

--------------------------------------------------------------------------------
-- * FFT functions
--------------------------------------------------------------------------------

-- | Wrapper for FFT algorithm
fft :: Int -> [S (Complex Double)] -> [S (Complex Double)]
fft = radix2

-- | Radix-2 decimation-in-time (DIT) FFT
radix2 :: Int -> [S (Complex Double)] -> [S (Complex Double)]
radix2 n = compose [stage i | i <- [1..n]] . bitRev n
  where
    stage i = bflys (i - 1) . raised (n - i) two (twid i)
    twid  i = one $ decmap (2 ^ (i - 1)) (wMult (2 ^ i))

-- | Base case of Butterfly computation
bfly :: [S (Complex Double)] -> [S (Complex Double)]
bfly [i1, i2] = [i1 + i2, i1 - i2]

-- | Recursive case of butterfly computation
bflys :: Int -> [S (Complex Double)] -> [S (Complex Double)]
bflys n = unriffle . raised n two bfly . riffle

-- | Bit reversal permutation
bitRev :: Int -> [a] -> [a]
bitRev n = compose [raised (n-i) two riffle | i <- [1..n] ] 
-- could accomplish this using more haskelly methods?

-- | Computes the 'twiddle factor'
w :: Int -> Int -> Int 
w n 0 = 1
w n k
   | k == n    = 1
   | otherwise = w (2 * n) (2 * k)
 
-- | Adding two signals
cplus :: S (Complex Double) -> S (Complex Double) -> S (Complex Double)
cplus x y = x + y

-- | Multiplying a signal by a constant
ctimes :: Int -> S (Complex Double) -> S (Complex Double)
ctimes a x = aSig * x
  where aCplx = 1.0 :: Expr (Complex Double)
        aSig  = S.repeat aCplx

-- | Multiplying a signal by 'w'
wMult :: Int -> Int -> S (Complex Double) -> S (Complex Double)
wMult n k a = ctimes twiddleFactor a
  where twiddleFactor = w n k


--------------------------------------------------------------------------------
-- * Helper functions
--------------------------------------------------------------------------------

-- | Splits a list in half; lists should be of even length.
splitTwo :: [a] -> ([a], [a])
splitTwo xs = ( take len xs, drop len xs )
    where len = length xs `div` 2

-- Construct function operating on 2n-lists from function operating on n-list
-- | Duplicates a list, applying the function f to the first half
one :: ([a] -> [a]) -> ([a] -> [a])
one f l = f l ++ l

-- Construct function operating on 2n-lists from function operating on n-list
-- | Duplicates a list, applying the function f to both halves
two :: ([a] -> [b]) -> ([a] -> [b])
two f l = f l ++ f l

-- | Applies a function f n times
raised :: Int -> (a -> a) -> (a -> a)
raised n f = (!! n) . iterate f

-- | Produces n-list of inputs by applying f (n - 1), f (n - 2), ..., f 0
--   conecutively
decmap :: Int -> (Int -> a -> b) -> ([a] -> [b])
decmap n f = zipWith f [n-i | i <- [1..n]]

-- | Composes a list of functions together
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

-- | 'Riffling' a stream, ...
riffle :: [a] -> [a]
riffle = riffle' . splitTwo

-- | ..., i.e. combining two input lists element by element; one list contains
--   all even-indexed elements, the other all odd-indexed elements
riffle' :: ([a], [a]) -> [a]
riffle' ([], [])         = []
riffle' (x:xs, y:ys) = x : y : riffle' (xs, ys)

-- | Separate an input list into two lists (even-indexed/odd-indexed elements)
unriffle :: [a] -> [a]
unriffle xs = as ++ bs
  where (as, bs) = unriffle' xs ([], [])

-- | Helper function for 'unriffle'
unriffle' :: [a] -> ([a], [a]) -> ([a], [a])
unriffle' []        acc    = acc
unriffle' (x:y:xs)  (p, q) = unriffle' xs (p ++ [x], q ++ [y])
unriffle' _          _     = error "illegal input"