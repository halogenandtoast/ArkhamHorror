module Arkham.Enemy.Cards.OldSadieSheldon (oldSadieSheldon) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype OldSadieSheldon = OldSadieSheldon EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldSadieSheldon :: EnemyCard OldSadieSheldon
oldSadieSheldon = enemy OldSadieSheldon Cards.oldSadieSheldon

instance HasModifiersFor OldSadieSheldon where
  getModifiersFor (OldSadieSheldon a) =
    modifySelect a (enemyIs Cards.sheldonGang) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities OldSadieSheldon where
  getAbilities (OldSadieSheldon a) = extend1 a $ restricted a 1 OnSameLocation parleyAction_

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
        select UneliminatedInvestigator >>= \case
          [iid'] -> do
            spendResources iid' 3
            addToVictory iid attrs
            remember TheDebtHasBeenPaid
          iids -> chooseInvestigatorAmounts iid "Resources to spend" total iids attrs
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      withInvestigatorAmounts choices spendResources
      addToVictory iid attrs
      remember TheDebtHasBeenPaid
      pure e
    _ -> OldSadieSheldon <$> liftRunMessage msg attrs
