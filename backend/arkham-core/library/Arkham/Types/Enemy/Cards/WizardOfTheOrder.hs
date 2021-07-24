module Arkham.Types.Enemy.Cards.WizardOfTheOrder
  ( WizardOfTheOrder(..)
  , wizardOfTheOrder
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message

newtype WizardOfTheOrder = WizardOfTheOrder EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfTheOrder :: EnemyCard WizardOfTheOrder
wizardOfTheOrder =
  enemy WizardOfTheOrder Cards.wizardOfTheOrder (4, Static 2, 2) (1, 0)

instance HasModifiersFor env WizardOfTheOrder

instance ActionRunner env => HasActions env WizardOfTheOrder where
  getActions i window (WizardOfTheOrder attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env WizardOfTheOrder where
  runMessage msg e@(WizardOfTheOrder attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EndMythos -> pure $ WizardOfTheOrder $ attrs & doomL +~ 1
    _ -> WizardOfTheOrder <$> runMessage msg attrs
