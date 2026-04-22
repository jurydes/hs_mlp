module Main where

import System.Environment (getArgs)

import DataLoader      (Dataset(..), loadDataset, numSamples, numFeatures)
import Perceptron      (ActivationFunction(..), PerceptronModel(..),
                        defaultConfig, trainPerceptron, predictAll)
import Evaluation      (computeAccuracy, normalizeToClass)
import ModelSerializer (saveModel, loadModel)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tf, vf]         -> run tf vf Nothing   StepFunction
    [tf, vf, mf]     -> run tf vf (Just mf) StepFunction
    [tf, vf, mf, an] ->
      case lookup an [("step", StepFunction), ("sign", SignFunction), ("sigmoid", SigmoidThreshold)] of
        Just act -> run tf vf (Just mf) act
        Nothing  -> die $ "Неизвестная функция активации: " ++ an
    _ -> die "Использование: perceptron <обуч.файл> <тест.файл> [файл_модели] [step|sign|sigmoid]"

trainAndReport :: ActivationFunction -> Dataset -> Maybe FilePath -> IO PerceptronModel
trainAndReport act trainData mFile = do
  let m = trainPerceptron (defaultConfig act)
            (datasetFeatureVectors trainData) (datasetLabels trainData)
  putStrLn $ "Обучено за " ++ show (pmEpochsTrained m) ++ " эпох."
  case mFile of
    Nothing -> return ()
    Just mf -> do
      saveModel mf m
      putStrLn ("Сохранено: " ++ mf)
  return m

run :: FilePath -> FilePath -> Maybe FilePath -> ActivationFunction -> IO ()
run trainFile testFile modelFile act = do
  trainData <- loadDataset trainFile
  testData  <- loadDataset testFile

  putStrLn $ "Обучающая: " ++ show (numSamples trainData) ++ " объектов, " ++ show (numFeatures trainData) ++ " признаков"
  putStrLn $ "Тестовая:  " ++ show (numSamples testData)  ++ " объектов, " ++ show (numFeatures testData)  ++ " признаков"
  putStrLn $ "Активация: " ++ show act

  if numFeatures trainData /= numFeatures testData && numFeatures testData /= 0
    then die "Число признаков в обучающей и тестовой выборках не совпадает"
    else do
      model <- case modelFile of
        Nothing -> trainAndReport act trainData Nothing
        Just mf -> do
          r <- loadModel mf
          case r of
            Just m  -> do
              putStrLn "Модель загружена."
              return m
            Nothing -> trainAndReport act trainData (Just mf)

      let trueLabels = datasetLabels testData
          preds      = map normalizeToClass (predictAll model (datasetFeatureVectors testData))
          acc        = computeAccuracy trueLabels preds
          accPct     = round (acc * 100) :: Int

      putStrLn "Предсказания (реальный -> предсказанный):"
      -- тут я спросил нейронку, как красиво отформатировать вывод, и она предложила так, промпт не нашел :(
      mapM_ (\(t, p) -> putStrLn $ show (round t :: Int) ++ " -> " ++ show (round p :: Int)
                                 ++ if t == p then "" else "  !") (zip trueLabels preds)

      putStrLn $ "Точность: " ++ show accPct ++ "%"

die :: String -> IO ()
die msg = putStrLn ("[ОШИБКА] " ++ msg)
