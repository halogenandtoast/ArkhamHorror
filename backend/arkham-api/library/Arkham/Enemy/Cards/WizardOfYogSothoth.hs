module Arkham.Enemy.Cards.WizardOfYogSothoth (wizardOfYogSothoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype WizardOfYogSothoth = WizardOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfYogSothoth :: EnemyCard WizardOfYogSothoth
wizardOfYogSothoth =
  enemy WizardOfYogSothoth Cards.wizardOfYogSothoth (4, Static 3, 3) (1, 2)
    & setPrey FewestCardsInHand

instance HasAbilities WizardOfYogSothoth where
  getAbilities (WizardOfYogSothoth x) =
    extend1 x
      $ restricted x 1 (thisExists x $ EnemyIsEngagedWith You)
      $ forced
      $ DrawCard #when You (basic $ hasAnyTrait [Hex, Pact]) AnyDeck

instance RunMessage WizardOfYogSothoth where
  runMessage msg e@(WizardOfYogSothoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> WizardOfYogSothoth <$> liftRunMessage msg attrs
