{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.ScreechingByakhee where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype ScreechingByakhee = ScreechingByakhee Attrs
  deriving newtype (Show, ToJSON, FromJSON)

screechingByakhee :: EnemyId -> ScreechingByakhee
screechingByakhee uuid =
  ScreechingByakhee
    $ baseAttrs uuid "01175"
    $ (healthDamage .~ 1)
    . (sanityDamage .~ 2)
    . (fight .~ 3)
    . (health .~ Static 4)
    . (evade .~ 3)
    . (prey .~ LowestRemainingSanity)

instance HasModifiersFor env ScreechingByakhee where
  getModifiersFor _ _ _ = pure []

instance (HasCount RemainingSanity InvestigatorId env) => HasModifiers env ScreechingByakhee where
  getModifiers _ (ScreechingByakhee Attrs {..}) = do
    sanities <- map unRemainingSanity
      <$> traverse (asks . getCount) (toList enemyEngagedInvestigators)
    let
      modifiers' =
        if any (<= 4) sanities then [EnemyFight 1, EnemyEvade 1] else []
    pure $ modifiers' <> concat (toList enemyModifiers)

instance ActionRunner env => HasActions env ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
