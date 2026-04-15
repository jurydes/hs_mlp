module DataLoader
  ( Dataset
  , loadDataset
  , numSamples
  , numFeatures
  , datasetFeatureVectors
  , datasetLabels
  ) where

import Data.Maybe            (mapMaybe)
import Text.Read             (readMaybe)
import Numeric.LinearAlgebra (Vector, fromList, size)

data Dataset = Dataset [Vector Double] [Double]
  deriving (Show)

loadDataset :: FilePath -> IO Dataset
loadDataset path = do
  contents <- readFile path
  let ls       = filter (\s -> not (null s) && head s /= '#') (lines contents)
      (xs, ys) = unzip (mapMaybe parseLine ls)
  return (Dataset xs ys)
  where
    parseLine line = do
      nums <- mapM readMaybe (words line)
      if length nums >= 2 then Just (fromList (init nums), last nums) else Nothing

numSamples :: Dataset -> Int
numSamples (Dataset xs _) = length xs

numFeatures :: Dataset -> Int
numFeatures (Dataset []    _) = 0
numFeatures (Dataset (v:_) _) = size v

datasetFeatureVectors :: Dataset -> [Vector Double]
datasetFeatureVectors (Dataset xs _) = xs

datasetLabels :: Dataset -> [Double]
datasetLabels (Dataset _ ys) = ys
