{-# LANGUAGE DefaultSignatures #-}
module Arkham.Types.Classes.HasTokenValue
  ( module Arkham.Types.Classes.HasTokenValue
  ) where

import Arkham.Prelude

import Arkham.Types.InvestigatorId
import Arkham.Types.Token
import GHC.Generics

class HasTokenValue1 env f where
  getTokenValue1 :: (MonadReader env m, MonadIO m) => f p -> InvestigatorId -> Token -> m TokenValue

instance (HasTokenValue1 env f) => HasTokenValue1 env (M1 i c f) where
  getTokenValue1 (M1 x) iid token = getTokenValue1 x iid token

instance (HasTokenValue1 env l, HasTokenValue1 env r) => HasTokenValue1 env (l :+: r) where
  getTokenValue1 (L1 x) iid token = getTokenValue1 x iid token
  getTokenValue1 (R1 x) iid token = getTokenValue1 x iid token

instance (HasTokenValue env p) => HasTokenValue1 env (K1 R p) where
  getTokenValue1 (K1 x) iid token = getTokenValue x iid token

class HasTokenValue env a where
  getTokenValue :: (MonadReader env m, MonadIO m) => a -> InvestigatorId -> Token -> m TokenValue
  default getTokenValue :: (Generic a, HasTokenValue1 env (Rep a), MonadReader env m, MonadIO m) => a -> InvestigatorId -> Token -> m TokenValue
  getTokenValue = defaultGetTokenValue

defaultGetTokenValue
  :: (Generic a, HasTokenValue1 env (Rep a), MonadReader env m, MonadIO m)
  => a
  -> InvestigatorId
  -> Token
  -> m TokenValue
defaultGetTokenValue a = getTokenValue1 (from a)

