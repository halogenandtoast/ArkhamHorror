module Arkham.Enemy.Cards.WizardOfYogSothoth (
  WizardOfYogSothoth (..),
  wizardOfYogSothoth,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype WizardOfYogSothoth = WizardOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfYogSothoth :: EnemyCard WizardOfYogSothoth
wizardOfYogSothoth =
  enemyWith
    WizardOfYogSothoth
    Cards.wizardOfYogSothoth
    (4, Static 3, 3)
    (1, 2)
    (preyL .~ Prey FewestCardsInHand)

instance HasAbilities WizardOfYogSothoth where
  getAbilities (WizardOfYogSothoth x) =
    withBaseAbilities
      x
      [ restrictedAbility x 1 (EnemyCriteria $ ThisEnemy $ EnemyIsEngagedWith You)
          $ ForcedAbility
          $ DrawCard
            Timing.When
            You
            (BasicCardMatch $ CardWithOneOf $ map CardWithTrait [Hex, Pact])
            AnyDeck
      ]

instance RunMessage WizardOfYogSothoth where
  runMessage msg e@(WizardOfYogSothoth attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e <$ push (EnemyAttack $ enemyAttack enemyId attrs iid)
    _ -> WizardOfYogSothoth <$> runMessage msg attrs
