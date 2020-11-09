{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.BogGator where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype BogGator = BogGator Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bogGator :: EnemyId -> BogGator
bogGator uuid =
  BogGator
    $ baseAttrs uuid "81022"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 1)
    . (fight .~ 2)
    . (health .~ Static 2)
    . (evade .~ 2)
    . (prey .~ LowestSkill SkillAgility)

instance EnemyRunner env => HasModifiersFor env BogGator where
  getModifiersFor _ (EnemyTarget eid) (BogGator Attrs {..}) | eid == enemyId =
    do
      bayouLocation <- asks $ member Bayou . getSet enemyLocation
      pure $ if bayouLocation then [EnemyFight 2, EnemyEvade 2] else []
  getModifiersFor _ _ _ = pure []

instance HasModifiers env BogGator where
  getModifiers _ (BogGator Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env BogGator where
  getActions i window (BogGator attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
