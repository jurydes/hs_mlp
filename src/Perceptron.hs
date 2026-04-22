module Perceptron
  ( ActivationFunction(..)
  , Weights(..)
  , Bias(..)
  , PerceptronConfig(..)
  , PerceptronModel(..)
  , defaultConfig
  , trainPerceptron
  , predictAll
  ) where

import Data.List             (foldl')
import Numeric.LinearAlgebra (Vector, fromList, toList, (<.>), size)

data ActivationFunction
  = StepFunction       -- ^ 0/1, порог в 0
  | SignFunction       -- ^ -1/+1, метки {-1, +1}
  | SigmoidThreshold   -- ^ σ(x) >= 0.5 → 1, иначе → 0
  deriving (Show, Read, Eq)

newtype Weights = Weights { unWeights :: Vector Double } deriving (Show)
newtype Bias    = Bias    { unBias    :: Double         } deriving (Show)

data PerceptronConfig = PerceptronConfig
  { pcActivation   :: ActivationFunction
  , pcLearningRate :: Double
  , pcMaxEpochs    :: Int
  } deriving (Show, Read)

data PerceptronModel = PerceptronModel
  { pmWeights       :: Weights
  , pmBias          :: Bias
  , pmConfig        :: PerceptronConfig
  , pmEpochsTrained :: Int
  } deriving (Show)

defaultConfig :: ActivationFunction -> PerceptronConfig
defaultConfig act = PerceptronConfig act 0.1 200

activate :: ActivationFunction -> Double -> Double
activate StepFunction      x = if x >= 0 then 1.0 else 0.0
activate SignFunction      x = if x >= 0 then 1.0 else -1.0
activate SigmoidThreshold x = if 1.0 / (1.0 + exp (-x)) >= 0.5 then 1.0 else 0.0

updateWeights :: ActivationFunction -> Double -> Weights -> Bias
             -> Vector Double -> Double -> (Weights, Bias, Bool)
updateWeights act eta (Weights w) (Bias b) x y
  | err == 0  = (Weights w, Bias b, False)
  | otherwise = ( Weights $ fromList $ zipWith (\wi xi -> wi + eta*err*xi) (toList w) (toList x)
                , Bias (b + eta * err)
                , True )
  where err = y - activate act (w <.> x + b)

trainEpoch :: ActivationFunction -> Double -> Weights -> Bias
           -> [Vector Double] -> [Double] -> (Weights, Bias, Bool)
trainEpoch act eta ws bs xs ys =
  foldl' step (ws, bs, False) (zip xs ys)
  where
    step (w, b, c) (x, y) =
      let (w', b', c') = updateWeights act eta w b x y
      in (w', b', c || c')

trainPerceptron :: PerceptronConfig -> [Vector Double] -> [Double] -> PerceptronModel
trainPerceptron cfg xs ys = go initW initB 0
  where
    act     = pcActivation cfg
    adapted = if act == SignFunction
              then map (\l -> if l > 0 then 1.0 else -1.0) ys
              else ys
    nFeats  = case xs of { [] -> 0; v:_ -> size v }
    initW   = Weights $ fromList $ replicate nFeats 0.0
    initB   = Bias 0.0
    go ws bs epoch
      | epoch >= pcMaxEpochs cfg = PerceptronModel ws bs cfg epoch
      | otherwise =
          let (ws', bs', changed) = trainEpoch act (pcLearningRate cfg) ws bs xs adapted
          in if changed
             then go ws' bs' (epoch + 1)
             else PerceptronModel ws' bs' cfg (epoch + 1)

predictAll :: PerceptronModel -> [Vector Double] -> [Double]
predictAll (PerceptronModel (Weights w) (Bias b) cfg _) =
  map (\x -> activate (pcActivation cfg) (w <.> x + b))
