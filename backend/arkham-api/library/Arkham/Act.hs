{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Act (Act (..), lookupAct) where

import Arkham.Act.Acts
import Arkham.Act.Types
import Arkham.Card
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Id
import Arkham.Prelude hiding (fold)
import Control.Monad.Fail (fail)

newtype MissingAct = MissingAct CardCode
  deriving stock (Show, Eq)

instance Exception MissingAct

lookupAct :: ActId -> Int -> CardId -> Either MissingAct Act
lookupAct actId = case lookup (unActId actId) allActs of
  Nothing -> \_ _ -> Left $ MissingAct (coerce actId)
  Just (SomeActCard a) -> \i cardId -> Right . Act $ cbCardBuilder a cardId (i, actId)

instance FromJSON Act where
  parseJSON = withObject "Act" $ \o -> do
    cCode <- o .: "id"
    case withActCardCode cCode (\(_ :: ActCard a) -> Act <$> parseJSON @a (Object o)) of
      Left (MissingAct c) -> fail $ "Unknown act: " <> show c
      Right a -> a

withActCardCode :: CardCode -> (forall a. IsAct a => ActCard a -> r) -> Either MissingAct r
withActCardCode cCode f = case lookup cCode allActs of
  Nothing -> Left $ MissingAct cCode
  Just (SomeActCard a) -> Right (f a)

allActs :: Map CardCode SomeActCard
allActs =
  (mapFrom someActCardCode Registry.acts <>)
    $ mapFrom someActCardCode allActCardBuilders
