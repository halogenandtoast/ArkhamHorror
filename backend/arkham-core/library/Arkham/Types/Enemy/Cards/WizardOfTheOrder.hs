{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.WizardOfTheOrder where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import ClassyPrelude

newtype WizardOfTheOrder = WizardOfTheOrder Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wizardOfTheOrder :: EnemyId -> WizardOfTheOrder
wizardOfTheOrder uuid = WizardOfTheOrder $ (baseAttrs uuid "01170")
  { enemyHealthDamage = 1
  , enemyFight = 4
  , enemyHealth = Static 2
  , enemyEvade = 2
  }

instance (EnemyRunner env) => RunMessage env WizardOfTheOrder where
  runMessage msg (WizardOfTheOrder attrs) =
    WizardOfTheOrder <$> runMessage msg attrs
