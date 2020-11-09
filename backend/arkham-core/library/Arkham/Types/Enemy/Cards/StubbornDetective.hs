{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.StubbornDetective where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

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

instance EnemyRunner env => HasModifiersFor env StubbornDetective where
  getModifiersFor _ (InvestigatorTarget iid) (StubbornDetective Attrs {..}) =
    do
      locationId <- asks $ getId @LocationId iid
      pure [ Blank | locationId == enemyLocation ]
  getModifiersFor _ _ _ = pure []

instance HasModifiers env StubbornDetective where
  getModifiers _ (StubbornDetective Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env StubbornDetective where
  getActions i window (StubbornDetective attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env StubbornDetective where
  runMessage msg (StubbornDetective attrs) =
    StubbornDetective <$> runMessage msg attrs
