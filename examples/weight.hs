{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Main where

import Control.Monad.Tangle
import GHC.Generics
import Barbies
import Control.Lens
import Control.Monad.IO.Class

data Info h = Info
  { _height :: h Double
  , _mass :: h Double
  , _bmi :: h Double
  } deriving Generic
instance FunctorB Info
instance ApplicativeB Info
makeLenses ''Info

buildInfo :: Info (TangleT Info IO)
buildInfo = Info
  { _height = liftIO $ putStr "Height(m): " >> readLn
  , _mass = liftIO $ putStr "Mass(kg): " >> readLn
  , _bmi = do
    h <- hitch height
    m <- hitch mass
    return $! m / (h * h)
  }

main :: IO ()
main = evalTangleT (hitch bmi) buildInfo (bpure Nothing) >>= print