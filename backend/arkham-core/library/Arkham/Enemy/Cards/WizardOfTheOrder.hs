module Arkham.Enemy.Cards.WizardOfTheOrder
  ( WizardOfTheOrder(..)
  , wizardOfTheOrder
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

newtype WizardOfTheOrder = WizardOfTheOrder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfTheOrder :: EnemyCard WizardOfTheOrder
wizardOfTheOrder = enemyWith
  WizardOfTheOrder
  Cards.wizardOfTheOrder
  (4, Static 2, 2)
  (1, 0)
  (spawnAtL ?~ SpawnLocation EmptyLocation)

instance HasAbilities WizardOfTheOrder where
  getAbilities (WizardOfTheOrder a) = withBaseAbilities
    a
    [ restrictedAbility a 1 (Negate $ SelfHasModifier CannotPlaceDoomOnThis)
      $ ForcedAbility
      $ PhaseEnds Timing.When
      $ PhaseIs MythosPhase
    ]

instance RunMessage WizardOfTheOrder where
  runMessage msg e@(WizardOfTheOrder attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      e <$ push (PlaceDoom (toTarget attrs) 1)
    _ -> WizardOfTheOrder <$> runMessage msg attrs
