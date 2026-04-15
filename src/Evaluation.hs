module Evaluation (computeAccuracy, normalizeToClass) where

normalizeToClass :: Double -> Double
normalizeToClass x = if x > 0.0 then 1.0 else 0.0

computeAccuracy :: [Double] -> [Double] -> Double
computeAccuracy ts ps
  | null ts   = 0.0
  | otherwise = fromIntegral (length (filter (uncurry (==)) (zip ts ps)))
              / fromIntegral (length ts)
