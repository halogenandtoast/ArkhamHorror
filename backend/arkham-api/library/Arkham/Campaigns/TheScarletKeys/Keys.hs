{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Keys (Key (..), createKey, lookupKey) where

import Arkham.Campaigns.TheScarletKeys.Key.Keys
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Card
import Arkham.Prelude hiding (Key, fold)
import Arkham.Target

createKey :: IsCard a => a -> Target -> KeyId -> Key
createKey a target sId = lookupKey sId target (toCardId a)

lookupKey :: KeyId -> Target -> CardId -> Key
lookupKey keyId = case lookup (unKeyId keyId) allKeys of
  Nothing -> error $ "Unknown key: " <> show keyId
  Just (SomeKeyCard a) -> \target cardId -> Key $ cbCardBuilder a cardId (target, keyId)

instance FromJSON Key where
  parseJSON = withObject "Key" $ \o -> do
    cCode <- o .: "id"
    withKeyCardCode cCode $ \(_ :: KeyCard a) -> Key <$> parseJSON @a (Object o)

withKeyCardCode :: CardCode -> (forall a. IsKey a => KeyCard a -> r) -> r
withKeyCardCode cCode f = case lookup cCode allKeys of
  Nothing -> error $ "Unknown key: " <> show cCode
  Just (SomeKeyCard a) -> f a

allKeys :: Map CardCode SomeKeyCard
allKeys =
  mapFrom
    someKeyCardCode
    [ -- Riddles and Rain
      SomeKeyCard theEyeOfRavens
    ]
