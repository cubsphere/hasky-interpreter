module Main where

import Front

main :: IO ()
main = putStrLn "Bem-vindo ao hasky-interpreter" >> (compute =<< getLine)
