{-# LANGUAGE OverloadedStrings #-}
module Arkham.BasicTypes where
  
import Data.Aeson
import Database.Persist.Sql
import GHC.Generics
import Prelude ((.), ($), Show, Read, Either(..), show)
import Data.Text (pack, unpack)
import Text.Read (readEither)
import Data.Bifunctor (first)

data Difficulty = Easy | Standard | Hard | Expert
  deriving stock (Show, Read, Generic)

instance FromJSON Difficulty
instance ToJSON Difficulty

instance PersistField Difficulty where
  toPersistValue = PersistText . pack . show
  fromPersistValue = \case
    PersistText t -> first pack . readEither . unpack $ t
    _ -> Left "invalid persist value"

instance PersistFieldSql Difficulty where
  sqlType _ = SqlString
