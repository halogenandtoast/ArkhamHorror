{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import System.FilePath
import Prelude
import Control.Monad (void)

main :: IO ()
main = do
    let url = "host=localhost dbname=arkham-horror-backend user=arkham-horror-backend password=arkham-horror-backend"
    con <- connectPostgreSQL url
    void $ withTransaction con $ runMigration $ MigrationContext commands True con
 where
   dir = takeDirectory __FILE__ <> "/migrations"
   commands = MigrationCommands
      [ MigrationInitialization
      , MigrationDirectory dir
      ]
