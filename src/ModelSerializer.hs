module ModelSerializer (saveModel, loadModel) where

import System.Directory      (doesFileExist)
import Data.List             (dropWhileEnd)
import Text.Read             (readMaybe)
import Numeric.LinearAlgebra (fromList, toList)
import Perceptron

saveModel :: FilePath -> PerceptronModel -> IO ()
saveModel path model = writeFile path $ serialize model

loadModel :: FilePath -> IO (Maybe PerceptronModel)
loadModel path = do
  exists <- doesFileExist path
  if exists
    then deserialize <$> readFile path
    else return Nothing

serialize :: PerceptronModel -> String
serialize model = unlines
  [ "PERCEPTRON_MODEL_V1"
  , "activation: "     ++ show (pcActivation   cfg)
  , "learning_rate: "  ++ show (pcLearningRate  cfg)
  , "max_epochs: "     ++ show (pcMaxEpochs     cfg)
  , "epochs_trained: " ++ show (pmEpochsTrained model)
  , "bias: "           ++ show (unBias $ pmBias model)
  , "weights: "        ++ unwords (map show $ toList $ unWeights $ pmWeights model)
  ]
  where cfg = pmConfig model

deserialize :: String -> Maybe PerceptronModel
deserialize content = case lines content of
  [] -> Nothing
  hdr : rest
    | hdr /= "PERCEPTRON_MODEL_V1" -> Nothing
    | otherwise -> do
        let splitKV s = let (k, r) = break (== ':') s
                            trim   = dropWhileEnd (== ' ') . dropWhile (== ' ')
                        in (trim k, dropWhile (== ' ') $ drop 1 r)
            fm    = map splitKV rest
            get k = lookup k fm
        actStr  <- get "activation"
        act     <- readMaybe actStr
        lrStr   <- get "learning_rate"
        lr      <- readMaybe lrStr
        meStr   <- get "max_epochs"
        me      <- readMaybe meStr
        etStr   <- get "epochs_trained"
        et      <- readMaybe etStr
        biasStr <- get "bias"
        bias    <- readMaybe biasStr
        wStrs   <- words <$> get "weights"
        ws    <- mapM readMaybe wStrs
        return $ PerceptronModel
          { pmWeights       = Weights $ fromList ws
          , pmBias          = Bias bias
          , pmConfig        = PerceptronConfig
                                { pcActivation   = act
                                , pcLearningRate = lr
                                , pcMaxEpochs    = me
                                }
          , pmEpochsTrained = et
          }
