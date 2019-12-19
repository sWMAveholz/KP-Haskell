{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Lib
import Data.Text

processIO :: Text -> IO ()
processIO line = do
  let res = parseAST line
  case res of
    Left err -> print err
    Right ex -> print (ex, eval ex)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "BoolParser> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (processIO (pack input)) >> loop