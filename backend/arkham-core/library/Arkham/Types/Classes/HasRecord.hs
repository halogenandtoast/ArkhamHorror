{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.HasRecord where

import Arkham.Prelude

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardCode
import GHC.Generics

class HasRecord env where
  hasRecord :: MonadReader env m => CampaignLogKey -> m Bool
  default hasRecord :: (MonadReader env m, Generic env, HasRecord1 (Rep env)) => CampaignLogKey -> m Bool
  hasRecord = defaultHasRecord
  hasRecordSet :: MonadReader env m => CampaignLogKey -> m [CardCode]
  default hasRecordSet :: (MonadReader env m, Generic env, HasRecord1 (Rep env)) => CampaignLogKey -> m [CardCode]
  hasRecordSet = defaultHasRecordSet
  hasRecordCount :: MonadReader env m => CampaignLogKey -> m Int
  default hasRecordCount :: (MonadReader env m, Generic env, HasRecord1 (Rep env)) => CampaignLogKey -> m Int
  hasRecordCount = defaultHasRecordCount

class HasRecord1 f where
  hasRecord1 :: MonadReader (f p) m => CampaignLogKey -> m Bool
  hasRecordSet1 :: MonadReader (f p) m => CampaignLogKey -> m [CardCode]
  hasRecordCount1 :: MonadReader (f p) m => CampaignLogKey -> m Int

instance HasRecord1 f => HasRecord1 (M1 i c f) where
  hasRecord1 logKey = do
    env <- ask
    case env of
      (M1 x) -> runReaderT (hasRecord1 logKey) x
  hasRecordSet1 logKey = do
    env <- ask
    case env of
      (M1 x) -> runReaderT (hasRecordSet1 logKey) x
  hasRecordCount1 logKey = do
    env <- ask
    case env of
      (M1 x) -> runReaderT (hasRecordCount1 logKey) x

instance (HasRecord1 l, HasRecord1 r) => HasRecord1 (l :+: r) where
  hasRecord1 logKey = do
    env <- ask
    case env of
      (L1 x) -> runReaderT (hasRecord1 logKey) x
      (R1 x) -> runReaderT (hasRecord1 logKey) x
  hasRecordSet1 logKey = do
    env <- ask
    case env of
      (L1 x) -> runReaderT (hasRecordSet1 logKey) x
      (R1 x) -> runReaderT (hasRecordSet1 logKey) x
  hasRecordCount1 logKey = do
    env <- ask
    case env of
      (L1 x) -> runReaderT (hasRecordCount1 logKey) x
      (R1 x) -> runReaderT (hasRecordCount1 logKey) x

instance HasRecord p => HasRecord1 (K1 R p) where
  hasRecord1 logKey = do
    env <- ask
    case env of
      (K1 x) -> runReaderT (hasRecord logKey) x
  hasRecordSet1 logKey = do
    env <- ask
    case env of
      (K1 x) -> runReaderT (hasRecordSet logKey) x
  hasRecordCount1 logKey = do
    env <- ask
    case env of
      (K1 x) -> runReaderT (hasRecordCount logKey) x

defaultHasRecord
  :: (MonadReader env m, Generic env, HasRecord1 (Rep env))
  => CampaignLogKey
  -> m Bool
defaultHasRecord logKey = do
  env <- from <$> ask
  runReaderT (hasRecord1 logKey) env

defaultHasRecordSet
  :: (MonadReader env m, Generic env, HasRecord1 (Rep env))
  => CampaignLogKey
  -> m [CardCode]
defaultHasRecordSet logKey = do
  env <- from <$> ask
  runReaderT (hasRecordSet1 logKey) env

defaultHasRecordCount
  :: (MonadReader env m, Generic env, HasRecord1 (Rep env))
  => CampaignLogKey
  -> m Int
defaultHasRecordCount logKey = do
  env <- from <$> ask
  runReaderT (hasRecordCount1 logKey) env

instance HasRecord a => HasRecord (a `With` b) where
  hasRecord logKey = do
    env <- asks withBase
    runReaderT (hasRecord logKey) env
  hasRecordSet logKey = do
    env <- asks withBase
    runReaderT (hasRecordSet logKey) env
  hasRecordCount logKey = do
    env <- asks withBase
    runReaderT (hasRecordCount logKey) env
