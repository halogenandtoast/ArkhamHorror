module Arkham.Types.Enemy.Cards.WizardOfYogSothoth
  ( WizardOfYogSothoth(..)
  , wizardOfYogSothoth
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype WizardOfYogSothoth = WizardOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfYogSothoth :: EnemyCard WizardOfYogSothoth
wizardOfYogSothoth = enemyWith
  WizardOfYogSothoth
  Cards.wizardOfYogSothoth
  (4, Static 3, 3)
  (1, 2)
  (preyL .~ FewestCards)

instance HasAbilities WizardOfYogSothoth where
  getAbilities (WizardOfYogSothoth x) = withBaseAbilities
    x
    [ restrictedAbility x 1 (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
      $ ForcedAbility
      $ DrawCard
          Timing.When
          You
          (BasicCardMatch $ CardWithOneOf $ map CardWithTrait [Hex, Pact])
          AnyDeck
    ]

instance EnemyRunner env => RunMessage env WizardOfYogSothoth where
  runMessage msg e@(WizardOfYogSothoth attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (EnemyAttack iid enemyId DamageAny)
    _ -> WizardOfYogSothoth <$> runMessage msg attrs
