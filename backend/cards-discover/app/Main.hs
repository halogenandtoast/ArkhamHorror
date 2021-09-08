module Main where

import Cards.Discover.Exe
import Options.Applicative
import Prelude

data Opts = Opts
  { dir :: FilePath
  , sourceName :: FilePath
  , sourceLocation :: FilePath
  , targetFile :: FilePath
  }

main :: IO ()
main = do
  Opts {..} <- execParser infoParser
  discoverCards (Source sourceName) (Destination targetFile) dir
 where
  infoParser = info
    (helper <*> optsParser)
    (progDesc "finds card files in a subdirectory and rexports them")
  optsParser =
    Opts
      <$> strOption (long "dir" <> short 'd' <> value "Cards")
      <*> argument str (metavar "NAME" <> help "Name of the source file")
      <*> argument str (metavar "PATH" <> help "Path to the input file")
      <*> argument str (metavar "PATH" <> help "Path to the output file")
