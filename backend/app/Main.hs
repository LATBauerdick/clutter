module Main where
import Relude

import App (startApp)

main :: IO ()
main = do
  startApp 8080
