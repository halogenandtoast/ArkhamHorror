module Arkham.Classes.Entity.Source where

import Arkham.Prelude hiding (to)

import Arkham.Source
import Arkham.Id

toAbilitySource :: SourceEntity a => a -> Int -> Source
toAbilitySource a = AbilitySource (toSource a)

isAbilitySource :: SourceEntity a => a -> Int -> Source -> Bool
isAbilitySource a idx source = case source of
  AbilitySource b idx' | idx == idx' -> isSource a b
  _ -> False

isProxiedAbilitySource :: SourceEntity a => a -> Int -> Source -> Bool
isProxiedAbilitySource a idx source = case source of
  AbilitySource p idx' | idx == idx' -> case p of
    ProxySource _ b -> isSource a b
    _ -> False
  _ -> False

class SourceEntity a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

instance SourceEntity Source where
  toSource = id
  isSource = (==)

instance SourceEntity a => SourceEntity (a `With` b) where
  toSource (a `With` _) = toSource a
  isSource (a `With` _) = isSource a

instance SourceEntity InvestigatorId where
  toSource = InvestigatorSource
