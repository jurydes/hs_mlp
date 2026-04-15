module ModelSerializer (saveModel, loadModel) where

import System.Directory      (doesFileExist)
import Data.List             (dropWhileEnd)
import Numeric.LinearAlgebra (fromList, toList)
import Perceptron

saveModel :: FilePath -> PerceptronModel -> IO ()
saveModel path model = writeFile path (serialize model)

loadModel :: FilePath -> IO (Maybe PerceptronModel)
loadModel path = do
  exists <- doesFileExist path
  if exists
    then Just . deserialize <$> readFile path
    else return Nothing

serialize :: PerceptronModel -> String
serialize model = unlines
  [ "PERCEPTRON_MODEL_V1"
  , "activation: "     ++ show (pcActivation   cfg)
  , "learning_rate: "  ++ show (pcLearningRate  cfg)
  , "max_epochs: "     ++ show (pcMaxEpochs     cfg)
  , "epochs_trained: " ++ show (pmEpochsTrained model)
  , "bias: "           ++ show (unBias (pmBias model))
  , "weights: "        ++ unwords (map show (toList (unWeights (pmWeights model))))
  ]
  where cfg = pmConfig model

deserialize :: String -> PerceptronModel
deserialize content =
  let fm      = map splitKV (tail (lines content))
      get k   = maybe (error $ "Отсутствует поле: " ++ k) id (lookup k fm)
      trim    = dropWhileEnd (== ' ') . dropWhile (== ' ')
      splitKV s = let (k, r) = break (== ':') s
                  in (trim k, dropWhile (== ' ') (drop 1 r))
  in PerceptronModel
       { pmWeights       = Weights (fromList (map read (words (get "weights"))))
       , pmBias          = Bias (read (get "bias"))
       , pmConfig        = PerceptronConfig
                             { pcActivation   = read (get "activation")
                             , pcLearningRate = read (get "learning_rate")
                             , pcMaxEpochs    = read (get "max_epochs")
                             }
       , pmEpochsTrained = read (get "epochs_trained")
       }
