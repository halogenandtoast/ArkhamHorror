{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.HasRecord where

import Arkham.Prelude

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import GHC.Generics

class HasRecord env a where
  hasRecord :: MonadReader env m => CampaignLogKey -> a -> m Bool
  default hasRecord :: (MonadReader env m, Generic a, HasRecord1 env (Rep a)) => CampaignLogKey -> a -> m Bool
  hasRecord = defaultHasRecord
  hasRecordSet :: MonadReader env m => CampaignLogKey -> a -> m [Recorded CardCode]
  default hasRecordSet :: (MonadReader env m, Generic a, HasRecord1 env (Rep a)) => CampaignLogKey -> a -> m [Recorded CardCode]
  hasRecordSet = defaultHasRecordSet
  hasRecordCount :: MonadReader env m => CampaignLogKey -> a -> m Int
  default hasRecordCount :: (MonadReader env m, Generic a, HasRecord1 env (Rep a)) => CampaignLogKey -> a -> m Int
  hasRecordCount = defaultHasRecordCount

class HasRecord1 env f where
  hasRecord1 :: MonadReader env m => CampaignLogKey -> f p -> m Bool
  hasRecordSet1 :: MonadReader env m => CampaignLogKey -> f p -> m [Recorded CardCode]
  hasRecordCount1 :: MonadReader env m => CampaignLogKey -> f p -> m Int

instance HasRecord1 env f => HasRecord1 env (M1 i c f) where
  hasRecord1 logKey (M1 x) = hasRecord1 logKey x
  hasRecordSet1 logKey (M1 x) = hasRecordSet1 logKey x
  hasRecordCount1 logKey (M1 x) = hasRecordCount1 logKey x

instance (HasRecord1 env l, HasRecord1 env r) => HasRecord1 env (l :+: r) where
  hasRecord1 logKey = \case
    (L1 x) -> hasRecord1 logKey x
    (R1 x) -> hasRecord1 logKey x
  hasRecordSet1 logKey = \case
    (L1 x) -> hasRecordSet1 logKey x
    (R1 x) -> hasRecordSet1 logKey x
  hasRecordCount1 logKey = \case
    (L1 x) -> hasRecordCount1 logKey x
    (R1 x) -> hasRecordCount1 logKey x

instance HasRecord env p => HasRecord1 env (K1 R p) where
  hasRecord1 logKey (K1 x) = hasRecord logKey x
  hasRecordSet1 logKey (K1 x) = hasRecordSet logKey x
  hasRecordCount1 logKey (K1 x) = hasRecordCount logKey x

defaultHasRecord
  :: (MonadReader env m, Generic a, HasRecord1 env (Rep a))
  => CampaignLogKey
  -> a
  -> m Bool
defaultHasRecord logKey = hasRecord1 logKey . from

defaultHasRecordSet
  :: (MonadReader env m, Generic a, HasRecord1 env (Rep a))
  => CampaignLogKey
  -> a
  -> m [Recorded CardCode]
defaultHasRecordSet logKey = hasRecordSet1 logKey . from

defaultHasRecordCount
  :: (MonadReader env m, Generic a, HasRecord1 env (Rep a))
  => CampaignLogKey
  -> a
  -> m Int
defaultHasRecordCount logKey = hasRecordCount1 logKey . from

instance HasRecord env a => HasRecord env (a `With` b) where
  hasRecord logKey = hasRecord logKey . withBase
  hasRecordSet logKey = hasRecordSet logKey . withBase
  hasRecordCount logKey = hasRecordCount logKey . withBase
