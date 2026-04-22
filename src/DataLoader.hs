{-# LANGUAGE NamedFieldPuns #-}
module DataLoader
  ( Dataset(..)
  , loadDataset
  , numSamples
  , numFeatures
  ) where

import Data.Maybe            (mapMaybe)
import Text.Read             (readMaybe)
import Numeric.LinearAlgebra (Vector, fromList, size)

data Dataset = Dataset
  { datasetFeatureVectors :: [Vector Double]
  , datasetLabels         :: [Double]
  } deriving (Show)

loadDataset :: FilePath -> IO Dataset
loadDataset path = do
  contents <- readFile path
  let ls       = filter (\s -> not (null s) && head s /= '#') $ lines contents
      (xs, ys) = unzip $ mapMaybe parseLine ls
  return $ Dataset { datasetFeatureVectors = xs, datasetLabels = ys }
  where
    parseLine line = do
      nums <- mapM readMaybe (words line)
      if length nums >= 2 then Just (fromList (init nums), last nums) else Nothing

numSamples :: Dataset -> Int
numSamples Dataset{datasetFeatureVectors} = length datasetFeatureVectors

numFeatures :: Dataset -> Int
numFeatures Dataset{datasetFeatureVectors = []}    = 0
numFeatures Dataset{datasetFeatureVectors = v : _} = size v
