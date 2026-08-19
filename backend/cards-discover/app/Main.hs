module Main where

import Cards.Discover.Exe
import Options.Applicative
import Prelude

data Opts = Opts
  { dir :: FilePath
  , only :: Maybe FilePath
  , instancesOnly :: Bool
  , homebrewContent :: Bool
  , homebrewDefs :: Bool
  , agendaBuilders :: Bool
  , agendaDefs :: Bool
  , sourceName :: FilePath
  , sourceLocation :: FilePath
  , targetFile :: FilePath
  }

main :: IO ()
main = do
  Opts {..} <- execParser infoParser
  let mode
        | agendaBuilders = AgendaBuilders
        | agendaDefs = AgendaDefs
        | homebrewContent = HomebrewContent
        | homebrewDefs = HomebrewCardDefs
        | instancesOnly = InstancesOnly
        | otherwise = ReExport
  discoverCardsWith (Source sourceName) (Destination targetFile) dir only mode
 where
  infoParser =
    info
      (helper <*> optsParser)
      (progDesc "finds card files in a subdirectory and rexports them")
  optsParser =
    Opts
      <$> strOption (long "dir" <> short 'd' <> value "Cards")
      <*> optional (strOption (long "only" <> help "Only files with this exact basename"))
      <*> switch (long "instances" <> help "Emit instance-only imports (import M ())")
      <*> switch (long "homebrew-content" <> help "Register discovered card builders as homebrew content")
      <*> switch (long "homebrew-defs" <> help "Register discovered card definitions as homebrew defs")
      <*> switch (long "agenda-builders" <> help "Emit allAgendaCardBuilders from discovered agenda modules")
      <*> switch (long "agenda-defs" <> help "Emit allAgendaCardDefs from discovered agenda card-def modules")
      <*> argument str (metavar "NAME" <> help "Name of the source file")
      <*> argument str (metavar "PATH" <> help "Path to the input file")
      <*> argument str (metavar "PATH" <> help "Path to the output file")
