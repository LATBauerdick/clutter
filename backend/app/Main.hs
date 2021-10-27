{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Relude
import System.Environment (getProgName, getArgs)
import System.IO (hPutStrLn, stderr)

import App (startApp)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Clutter app starting... arguments are:"
  name <- getProgName
  hPutStrLn stderr $ "usage: " ++ name ++ " <port number>"

  let p = case args of
         [ps] -> fromMaybe 8080 $ readMaybe ps :: Int
         _ -> 8080

  startApp p
