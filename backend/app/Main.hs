{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ViewPatterns #-}

module Main where

import Relude
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn)
import qualified Relude.Unsafe as Unsafe
-- import qualified Data.Text as T

import App (startApp)

main :: IO ()
main = System.Environment.getArgs >>= parse >>= process False

parse :: [String] -> IO [String]
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse [] = return ["-c"] :: IO [String]
parse x = return x :: IO [String]

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ "Usage: " <> name <> " [-c(ache)] [port]"
version :: IO ()
version = putStrLn "clutter v. 0.4.2"

readInt :: String -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe

process :: Bool -> [String] -> IO ()
process cache args =
  case args of
    [] -> do
      startApp 8080 cache
    "-c":rest -> do
      putStrLn "using cached JSON from Discogs"
      process True rest
    [port] -> do
      let p = readInt port
      startApp p cache
    _ -> do
      putStrLn "??????"
      exitSuccess

main' :: IO ()
main' = do
  args <- System.Environment.getArgs
  putStrLn "Clutter app starting... arguments are:"
  name <- getProgName
  hPutStrLn stderr $ "usage: " ++ name ++ " <port number>"

  let p = case args of
         [ps] -> fromMaybe 8080 $ readMaybe ps :: Int
         _ -> 8080

  startApp p False
