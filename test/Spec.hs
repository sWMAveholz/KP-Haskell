{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Monad.Trans
import System.Console.Haskeline
import Lib
import Data.Text

main :: IO ()
main = do
    let res1 = parseAST "T|(True&true)&!F"
    case res1 of
        Left err -> print err
        Right ex -> if eval ex == True
            then print "test nr. 1: success" 
            else print "test nr. 1: failed"

    let res2 = parseAST "T"
    case res2 of
        Left err -> print err
        Right ex -> if eval ex == True
            then print "test nr. 2: success" 
            else print "test nr. 2: failed"    

    let res2 = parseAST "!  T"
    case res2 of
        Left err -> print err
        Right ex -> if eval ex == False
            then print "test nr. 3: success" 
            else print "test nr. 3: failed" 

    let res3 = parseAST "!F     &(T    &F)     |T   "
    case res3 of
        Left err -> print err
        Right ex -> if eval ex == True
            then print "test nr. 4: success" 
            else print "test nr. 4: failed"