{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Relude
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn)

import App (startApp)

main :: IO ()
main = do
  args <- System.Environment.getArgs
  putStrLn "Clutter app starting... arguments are:"
  name <- getProgName
  hPutStrLn stderr $ "usage: " ++ name ++ " <port number>"

  let p = case args of
         [ps] -> fromMaybe 8080 $ readMaybe ps :: Int
         _ -> 8080

  startApp p
