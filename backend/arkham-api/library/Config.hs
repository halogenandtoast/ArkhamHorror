{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some next-gen helper functions for the scaffolding's configuration system.
module Config
    ( -- * Locally defined
      configSettingsYml
    , getDevSettings
    , develMainHelper
    , makeYesodLogger
      -- * Re-exports from Data.Yaml.Config
    , applyCurrentEnv
    , getCurrentEnv
    , applyEnvValue
    , loadYamlSettings
    , loadYamlSettingsArgs
    , EnvUsage
    , ignoreEnv
    , useEnv
    , requireEnv
    , useCustomEnv
    , requireCustomEnv
    ) where

import Import.NoFoundation

import Data.Yaml.Config
import System.Environment (getEnvironment)
import Network.Wai.Handler.Warp
import Text.Read (readMaybe)
import Control.Concurrent (forkIO, threadDelay)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)
import Network.Wai.Logger (clockDateCacher)
import Yesod.Core.Types (Logger (Logger))
import System.Log.FastLogger (LoggerSet)
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))

-- | Location of the default config file.
configSettingsYml :: FilePath
configSettingsYml = "config/settings.yml"

-- | Helper for getApplicationDev in the scaffolding. Looks up PORT and
-- DISPLAY_PORT and prints appropriate messages.
getDevSettings :: Settings -> IO Settings
getDevSettings settings = do
    env <- getEnvironment
    let p = fromMaybe (getPort settings) $ lookup "PORT" env >>= readMaybe
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
    putStrLn $ "Devel application launched: http://localhost:" ++ tshow pdisplay
    return $ setPort p settings

-- | Helper for develMain in the scaffolding.
develMainHelper :: IO (Settings, Application) -> IO ()
develMainHelper getSettingsApp = do
    _ <- installHandler sigINT (Catch $ return ()) Nothing
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

-- | Create a 'Logger' value (from yesod-core) out of a 'LoggerSet' (from
-- fast-logger).
makeYesodLogger :: LoggerSet -> IO Logger
makeYesodLogger loggerSet' = do
    (getter, _) <- clockDateCacher
    return $! Yesod.Core.Types.Logger loggerSet' getter
