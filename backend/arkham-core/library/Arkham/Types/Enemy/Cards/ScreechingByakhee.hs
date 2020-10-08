{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.ScreechingByakhee where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import ClassyPrelude

newtype ScreechingByakhee = ScreechingByakhee Attrs
  deriving newtype (Show, ToJSON, FromJSON)

screechingByakhee :: EnemyId -> ScreechingByakhee
screechingByakhee uuid = ScreechingByakhee $ (baseAttrs uuid "01175")
  { enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 3
  , enemyPrey = LowestRemainingSanity
  }

instance HasModifiersFor env investigator ScreechingByakhee where
  getModifiersFor _ _ _ = pure []

instance (HasCount RemainingSanity InvestigatorId env) => HasModifiers env ScreechingByakhee where
  getModifiers _ (ScreechingByakhee Attrs {..}) = do
    sanities <- map unRemainingSanity
      <$> traverse (asks . getCount) (toList enemyEngagedInvestigators)
    let
      modifiers' =
        if any (<= 4) sanities then [EnemyFight 1, EnemyEvade 1] else []
    pure $ modifiers' <> concat (toList enemyModifiers)

instance (IsInvestigator investigator) => HasActions env investigator ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
