{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Keys (ScarletKey (..), createScarletKey, lookupScarletKey) where

import Arkham.Campaigns.TheScarletKeys.Key.Keys
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Card
import Arkham.Prelude hiding (fold)
import Arkham.Target

createScarletKey :: IsCard a => a -> Target -> ScarletKeyId -> ScarletKey
createScarletKey a target sId = lookupScarletKey sId target (toCardId a)

lookupScarletKey :: ScarletKeyId -> Target -> CardId -> ScarletKey
lookupScarletKey keyId = case lookup (unScarletKeyId keyId) allScarletKeys of
  Nothing -> error $ "Unknown key: " <> show keyId
  Just (SomeScarletKeyCard a) -> \target cardId -> ScarletKey $ cbCardBuilder a cardId (target, keyId)

instance FromJSON ScarletKey where
  parseJSON = withObject "ScarletKey" $ \o -> do
    cCode <- o .: "id"
    withScarletKeyCardCode cCode $ \(_ :: ScarletKeyCard a) -> ScarletKey <$> parseJSON @a (Object o)

withScarletKeyCardCode :: CardCode -> (forall a. IsScarletKey a => ScarletKeyCard a -> r) -> r
withScarletKeyCardCode cCode f = case lookup cCode allScarletKeys of
  Nothing -> error $ "Unknown key: " <> show cCode
  Just (SomeScarletKeyCard a) -> f a

allScarletKeys :: Map CardCode SomeScarletKeyCard
allScarletKeys =
  mapFrom
    someScarletKeyCardCode
    [ -- Riddles and Rain
      SomeScarletKeyCard theEyeOfRavens
    , -- Dead Heat
      SomeScarletKeyCard theLastBlossom
    ]
