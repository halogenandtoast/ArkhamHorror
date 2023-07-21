{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Some next-gen helper functions for the scaffolding's configuration system.
module Config (
  -- * Locally defined
  configSettingsYml,
  getDevSettings,
  develMainHelper,
  makeYesodLogger,

  -- * Re-exports from Data.Yaml.Config
  applyCurrentEnv,
  getCurrentEnv,
  applyEnvValue,
  loadYamlSettings,
  loadYamlSettingsArgs,
  EnvUsage,
  ignoreEnv,
  useEnv,
  requireEnv,
  useCustomEnv,
  requireCustomEnv,
) where

import Import.NoFoundation hiding (exitSuccess)

import Control.Concurrent (forkIO, threadDelay)
import Data.List (lookup)
import Data.Yaml.Config
import Network.Wai.Handler.Warp
import Network.Wai.Logger (clockDateCacher)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (exitSuccess)
import System.Log.FastLogger (LoggerSet)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)
import Yesod.Core.Types (Logger (Logger))

-- | Location of the default config file.
configSettingsYml :: FilePath
configSettingsYml = "config/settings.yml"

{- | Helper for getApplicationDev in the scaffolding. Looks up PORT and
 DISPLAY_PORT and prints appropriate messages.
-}
getDevSettings :: Settings -> IO Settings
getDevSettings settings = do
  env <- getEnvironment
  let p = fromMaybe (getPort settings) $ lookup "PORT" env >>= readMaybe
      pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
  putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
  pure $ setPort p settings

-- | Helper for develMain in the scaffolding.
develMainHelper :: IO (Settings, Application) -> IO ()
develMainHelper getSettingsApp = do
  _ <- installHandler sigINT (Catch $ pure ()) Nothing
  putStrLn "Starting devel application"
  (settings, app) <- getSettingsApp
  _ <- forkIO $ runSettings settings app
  loop
 where
  loop :: IO ()
  loop = do
    threadDelay 100000
    e <- doesFileExist "yesod-devel/devel-terminate"
    if e then terminateDevel else loop

  terminateDevel :: IO ()
  terminateDevel = exitSuccess

{- | Create a 'Logger' value (from yesod-core) out of a 'LoggerSet' (from
 fast-logger).
-}
makeYesodLogger :: LoggerSet -> IO Logger
makeYesodLogger loggerSet' = do
  (getter, _) <- clockDateCacher
  pure $! Yesod.Core.Types.Logger loggerSet' getter
