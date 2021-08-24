module Arkham.Types.Enemy.Cards.WizardOfTheOrder
  ( WizardOfTheOrder(..)
  , wizardOfTheOrder
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import qualified Arkham.Types.Timing as Timing

newtype WizardOfTheOrder = WizardOfTheOrder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfTheOrder :: EnemyCard WizardOfTheOrder
wizardOfTheOrder = enemyWith
  WizardOfTheOrder
  Cards.wizardOfTheOrder
  (4, Static 2, 2)
  (1, 0)
  (spawnAtL ?~ EmptyLocation)

instance HasAbilities env WizardOfTheOrder where
  getAbilities _ _ (WizardOfTheOrder a) = pure
    [ mkAbility a 1 $ ForcedAbility $ PhaseEnds Timing.After $ PhaseIs
        MythosPhase
    ]

instance EnemyRunner env => RunMessage env WizardOfTheOrder where
  runMessage msg e@(WizardOfTheOrder attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (PlaceDoom (toTarget attrs) 1)
    _ -> WizardOfTheOrder <$> runMessage msg attrs
