module Arkham.Classes.Entity.Source where

import Arkham.Prelude hiding ( to )

import Arkham.Id
import Arkham.Source

class Sourceable a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

isProxySource :: Sourceable a => a -> Source -> Bool
isProxySource a (ProxySource _ source) = isSource a source
isProxySource _ _ = False

toProxySource :: Sourceable a => a -> Source -> Source
toProxySource a source = ProxySource source (toSource a)

isSkillTestSource :: Sourceable a => a -> Source -> Bool
isSkillTestSource a = \case
  SkillTestSource _ _ source _ -> a `isSource` source
  _ -> False

instance Sourceable Source where
  toSource = id
  isSource = (==)

instance Sourceable a => Sourceable (a `With` b) where
  toSource (a `With` _) = toSource a
  isSource (a `With` _) = isSource a

instance Sourceable InvestigatorId where
  toSource = InvestigatorSource

toAbilitySource :: Sourceable a => a -> Int -> Source
toAbilitySource = AbilitySource . toSource

isAbilitySource :: Sourceable a => a -> Int -> Source -> Bool
isAbilitySource a idx (AbilitySource b idx') | idx == idx' = isSource a b
isAbilitySource _ _ _ = False
