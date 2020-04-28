{-# LANGUAGE CPP #-}
import Control.Monad (void)
import Database.Persist.Postgresql hiding (runMigration)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Data.Yaml.Config
import Prelude
import System.FilePath
import Yesod.Default.Config2

import Settings

main :: IO ()
main = do
    settings <- loadYamlSettings [configSettingsYml] [] useEnv
    con <- connectPostgreSQL $ pgConnStr $ appDatabaseConf settings
    void $ withTransaction con $ runMigration $ MigrationContext commands True con
 where
   dir = takeDirectory __FILE__ <> "/migrations"
   commands = MigrationCommands
      [ MigrationInitialization
      , MigrationDirectory dir
      ]
