{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.HasRecord where

import Arkham.Prelude

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import GHC.Generics

class HasRecord a where
  hasRecord :: CampaignLogKey -> a -> Bool
  default hasRecord :: (Generic a, HasRecord1 (Rep a)) => CampaignLogKey -> a -> Bool
  hasRecord = defaultHasRecord
  hasRecordSet :: CampaignLogKey -> a -> [CardCode]
  default hasRecordSet :: (Generic a, HasRecord1 (Rep a)) => CampaignLogKey -> a -> [CardCode]
  hasRecordSet = defaultHasRecordSet

class HasRecord1 f where
  hasRecord1 :: CampaignLogKey -> f p -> Bool
  hasRecordSet1 :: CampaignLogKey -> f p -> [CardCode]

instance HasRecord1 f => HasRecord1 (M1 i c f) where
  hasRecord1 logKey (M1 x) = hasRecord1 logKey x
  hasRecordSet1 logKey (M1 x) = hasRecordSet1 logKey x

instance (HasRecord1 l, HasRecord1 r) => HasRecord1 (l :+: r) where
  hasRecord1 logKey (L1 x) = hasRecord1 logKey x
  hasRecord1 logKey (R1 x) = hasRecord1 logKey x
  hasRecordSet1 logKey (L1 x) = hasRecordSet1 logKey x
  hasRecordSet1 logKey (R1 x) = hasRecordSet1 logKey x

instance HasRecord p => HasRecord1 (K1 R p) where
  hasRecord1 logKey (K1 x) = hasRecord logKey x
  hasRecordSet1 logKey (K1 x) = hasRecordSet logKey x

defaultHasRecord
  :: (Generic a, HasRecord1 (Rep a)) => CampaignLogKey -> a -> Bool
defaultHasRecord logKey = hasRecord1 logKey . from

defaultHasRecordSet
  :: (Generic a, HasRecord1 (Rep a)) => CampaignLogKey -> a -> [CardCode]
defaultHasRecordSet logKey = hasRecordSet1 logKey . from

instance HasRecord a => HasRecord (a `With` b) where
  hasRecord logKey (a `With` _) = hasRecord logKey a
  hasRecordSet logKey (a `With` _) = hasRecordSet logKey a
