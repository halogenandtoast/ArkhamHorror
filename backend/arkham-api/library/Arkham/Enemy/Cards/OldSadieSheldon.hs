module Arkham.Enemy.Cards.OldSadieSheldon (oldSadieSheldon) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MachinationsThroughTime.Helpers

newtype OldSadieSheldon = OldSadieSheldon EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldSadieSheldon :: EnemyCard OldSadieSheldon
oldSadieSheldon = enemy OldSadieSheldon Cards.oldSadieSheldon (1, Static 1, 1) (1, 0)

instance HasModifiersFor OldSadieSheldon where
  getModifiersFor (OldSadieSheldon a) =
    modifySelect a (enemyIs Cards.sheldonGang) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities OldSadieSheldon where
  getAbilities (OldSadieSheldon a) =
    extend1 a $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage OldSadieSheldon where
  runMessage msg e@(OldSadieSheldon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      total <- perPlayer 3
      totalResources <- selectSum InvestigatorResources UneliminatedInvestigator
      when (totalResources >= total) do
        chooseOneM iid $ withI18n do
          labeled' "skip" nothing
          scenarioI18n $ labeled' "oldSadieSheldon.payTheDebt" do
            iids <- select UneliminatedInvestigator
            chooseInvestigatorAmounts iid "Resources to spend" total iids attrs
      pure e
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      withInvestigatorAmounts choices \iid n -> push $ SpendResources iid n
      lead <- getLead
      addToVictory lead attrs
      remember TheDebtHasBeenPaid
      pure e
    _ -> OldSadieSheldon <$> liftRunMessage msg attrs
