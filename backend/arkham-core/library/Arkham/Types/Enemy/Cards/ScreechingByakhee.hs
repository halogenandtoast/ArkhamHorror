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

instance HasCount RemainingSanity env InvestigatorId => HasModifiersFor env ScreechingByakhee where
  getModifiersFor _ target (ScreechingByakhee attrs) | isTarget attrs target =
    do
      sanities <- map unRemainingSanity
        <$> traverse getCount (setToList $ enemyEngagedInvestigators attrs)
      pure $ if any (<= 4) sanities then [EnemyFight 1, EnemyEvade 1] else []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
