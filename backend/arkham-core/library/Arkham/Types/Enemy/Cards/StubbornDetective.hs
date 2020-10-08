{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.StubbornDetective where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Modifier
import Arkham.Types.Prey
import ClassyPrelude

newtype StubbornDetective = StubbornDetective Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stubbornDetective :: EnemyId -> StubbornDetective
stubbornDetective uuid = StubbornDetective $ (weaknessBaseAttrs uuid "01102")
  { enemyHealthDamage = 1
  , enemySanityDamage = 0
  , enemyFight = 3
  , enemyHealth = Static 2
  , enemyEvade = 2
  , enemyPrey = SetToBearer
  }

instance IsInvestigator investigator => HasModifiersFor env investigator StubbornDetective where
  getModifiersFor _ i (StubbornDetective Attrs {..}) =
    pure [ Blank | locationOf i == enemyLocation ]

instance HasModifiers env StubbornDetective where
  getModifiers _ (StubbornDetective Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance (IsInvestigator investigator) => HasActions env investigator StubbornDetective where
  getActions i window (StubbornDetective attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env StubbornDetective where
  runMessage msg (StubbornDetective attrs) =
    StubbornDetective <$> runMessage msg attrs
