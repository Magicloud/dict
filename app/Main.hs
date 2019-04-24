module Main where

import Data.List
import Dict.Bing
import Dict.Types
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ (\w -> do
  di <- translate w
  putStrLn $ diWord di ++ "\t" ++ intercalate " " (diPronounces di)
  mapM_ (\m -> putStrLn $ fst m ++ "\t" ++ snd m) $ diMeanings di)
