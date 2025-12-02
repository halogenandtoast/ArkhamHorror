module Arkham.Location.Cards.ZanEtElSettat (zanEtElSettat) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.DogsOfWar.Helpers

newtype ZanEtElSettat = ZanEtElSettat LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zanEtElSettat :: LocationCard ZanEtElSettat
zanEtElSettat = symbolLabel $ location ZanEtElSettat Cards.zanEtElSettat 0 (Static 0)

instance HasAbilities ZanEtElSettat where
  getAbilities (ZanEtElSettat a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ forced
      $ InitiatedSkillTest #when You #any #any (WhileAttackingAnEnemy AnyEnemy)

instance RunMessage ZanEtElSettat where
  runMessage msg l@(ZanEtElSettat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actions <- field InvestigatorRemainingActions iid
      chooseOneM iid $ scenarioI18n do
        unscoped
          $ countVar 1
          $ labeledValidate' (actions > 0) "spendActions"
          $ spendActions iid (attrs.ability 1) 1
        labeled' "zanEtElSettat.increaseFight" do
          whenJustM getSkillTest \st ->
            skillTestModifier st.id (attrs.ability 1) st.target (EnemyFight 2)
      pure l
    _ -> ZanEtElSettat <$> liftRunMessage msg attrs
