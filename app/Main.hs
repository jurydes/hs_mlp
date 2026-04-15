module Main where

import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(..))
import Control.Monad      (when)

import DataLoader      (loadDataset, numSamples, numFeatures,
                        datasetFeatureVectors, datasetLabels)
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

run :: FilePath -> FilePath -> Maybe FilePath -> ActivationFunction -> IO ()
run trainFile testFile modelFile act = do
  trainData <- loadDataset trainFile
  testData  <- loadDataset testFile

  putStrLn $ "Обучающая: " ++ show (numSamples trainData) ++ " объектов, " ++ show (numFeatures trainData) ++ " признаков"
  putStrLn $ "Тестовая:  " ++ show (numSamples testData)  ++ " объектов, " ++ show (numFeatures testData)  ++ " признаков"
  putStrLn $ "Активация: " ++ show act

  when (numFeatures trainData /= numFeatures testData && numFeatures testData /= 0) $
    die "Число признаков в обучающей и тестовой выборках не совпадает"

  let trained = trainPerceptron (defaultConfig act)
                  (datasetFeatureVectors trainData) (datasetLabels trainData)

  model <- case modelFile of
    Nothing -> do
      putStrLn $ "Обучено за " ++ show (pmEpochsTrained trained) ++ " эпох."
      return trained
    Just mf -> do
      r <- loadModel mf
      case r of
        Just m  -> putStrLn "Модель загружена." >> return m
        Nothing -> do
          putStrLn $ "Обучено за " ++ show (pmEpochsTrained trained) ++ " эпох."
          saveModel mf trained
          putStrLn $ "Сохранено: " ++ mf
          return trained

  let trueLabels  = datasetLabels testData
      preds       = map normalizeToClass (predictAll model (datasetFeatureVectors testData))
      acc         = computeAccuracy trueLabels preds
      accPct      = round (acc * 100) :: Int

  putStrLn "Предсказания (реальный -> предсказанный):"
  mapM_ (\(t, p) -> putStrLn $ show (round t :: Int) ++ " -> " ++ show (round p :: Int)
                             ++ if t == p then "" else "  !") (zip trueLabels preds)

  putStrLn $ "Точность: " ++ show accPct ++ "%"

die :: String -> IO a
die msg = putStrLn ("[ОШИБКА] " ++ msg) >> exitWith (ExitFailure 1)
