module Main where

import TrainGame
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args <= 1 then
    putStrLn "Usage: ./trainGame [0-9] [0-9]+" 
  else
    let numbers = map (\x -> read x :: Integer) args in
    mapM_ putStrLn $ solutions numbers