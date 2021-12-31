{-# LANGUAGE DefaultSignatures #-}
module Arkham.Classes.HasTokenValue
  ( module Arkham.Classes.HasTokenValue
  ) where

import Arkham.Prelude

import Arkham.InvestigatorId
import Arkham.Projection
import Arkham.Location.Attrs
import Arkham.Token
import GHC.Generics

type TokenValueM env m = (MonadReader env m, Projection env LocationAttrs)

class HasTokenValue1 env f where
  getTokenValue1 :: TokenValueM env m => f p -> InvestigatorId -> TokenFace -> m TokenValue

instance (HasTokenValue1 env f) => HasTokenValue1 env (M1 i c f) where
  getTokenValue1 (M1 x) iid token = getTokenValue1 x iid token

instance (HasTokenValue1 env l, HasTokenValue1 env r) => HasTokenValue1 env (l :+: r) where
  getTokenValue1 (L1 x) iid token = getTokenValue1 x iid token
  getTokenValue1 (R1 x) iid token = getTokenValue1 x iid token

instance (HasTokenValue env p) => HasTokenValue1 env (K1 R p) where
  getTokenValue1 (K1 x) iid token = getTokenValue x iid token

class HasTokenValue env a where
  getTokenValue :: TokenValueM env m => a -> InvestigatorId -> TokenFace -> m TokenValue
  default getTokenValue :: (Generic a, HasTokenValue1 env (Rep a), TokenValueM env m) => a -> InvestigatorId -> TokenFace -> m TokenValue
  getTokenValue = defaultGetTokenValue

defaultGetTokenValue
  :: (Generic a, HasTokenValue1 env (Rep a), TokenValueM env m)
  => a
  -> InvestigatorId
  -> TokenFace
  -> m TokenValue
defaultGetTokenValue a = getTokenValue1 (from a)
