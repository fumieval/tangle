{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Main where

import Barbies
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Tangle
import GHC.Generics
import System.IO (hFlush, stdout)

data Info h = Info
  { _height :: h Double
  , _mass :: h Double
  , _bmi :: h Double
  , _isHealthy :: h Bool
  } deriving Generic
instance FunctorB Info
instance ApplicativeB Info
makeLenses ''Info

prompt :: Read a => String -> IO a
prompt str = do
  putStr str
  hFlush stdout
  readLn

buildInfo :: Info (TangleT Info IO)
buildInfo = Info
  { _height = liftIO $ prompt "Height(m): "
  , _mass = liftIO $ prompt "Mass(kg): "
  , _bmi = do
    h <- hitch height
    m <- hitch mass
    return $! m / (h * h)
  , _isHealthy = do
    x <- hitch bmi
    pure $ x >= 18 && x < 22
  }

main :: IO ()
main = evalTangleT (hitch bmi) buildInfo >>= print