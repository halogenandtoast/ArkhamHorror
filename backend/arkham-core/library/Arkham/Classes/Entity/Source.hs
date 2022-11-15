module Arkham.Classes.Entity.Source where

import Arkham.Prelude hiding (to)

import Arkham.Source
import Arkham.Id

class SourceEntity a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

isSkillTestSource :: SourceEntity a => a -> Source -> Bool
isSkillTestSource a = \case
  SkillTestSource _ _ source _ -> a `isSource` source
  _ -> False

instance SourceEntity Source where
  toSource = id
  isSource = (==)

instance SourceEntity a => SourceEntity (a `With` b) where
  toSource (a `With` _) = toSource a
  isSource (a `With` _) = isSource a

instance SourceEntity InvestigatorId where
  toSource = InvestigatorSource

toAbilitySource :: SourceEntity a => a -> Int -> Source
toAbilitySource = AbilitySource . toSource

isAbilitySource :: SourceEntity a => a -> Int -> Source -> Bool
isAbilitySource a idx (AbilitySource b idx') | idx == idx' = isSource a b
isAbilitySource _ _ _ = False
