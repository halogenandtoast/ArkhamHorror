module Utility where

import Data.Aeson
import Prelude (Maybe, Monad, String, maybe, pure, id, (.), otherwise, show, (<>), ($), error, length, drop)
import Data.List (isPrefixOf)
import qualified Data.Char as C

snakeCaseify :: String -> String
snakeCaseify [] = []
snakeCaseify (c:cs) | C.isLower c = c : snakeCaseify cs
                    | otherwise = '_' : C.toLower c : snakeCaseify cs

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = C.toLower c : cs

aesonOptions :: Maybe String -> Options
aesonOptions mPrefix = defaultOptions
  { constructorTagModifier = snakeCaseify . unCapitalize . modify
  , fieldLabelModifier = unCapitalize . modify
  }
 where
  modify = maybe id dropPrefix mPrefix

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM nothingAction = maybe nothingAction pure

dropPrefix :: String -> String -> String
dropPrefix prefix x =
  if prefix `isPrefixOf` x
    then drop (length prefix) x
    else error $ "dropPrefix: " <> show prefix <> " is not a prefix of " <> show x
