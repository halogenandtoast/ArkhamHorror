module Main where

import Cards.Discover.Exe
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    src : _ : dest : _ -> discoverCards (Source src) (Destination dest)
    _ ->
      fail
        . mconcat
        $ [ "cards-discover: expected to be called with three arguments as an -F -pgmF preprocessor. got:"
          , show args
          ]
