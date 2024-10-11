module Arkham.Asset.Assets.Kukri (kukri, Kukri (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype Kukri = Kukri AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kukri :: AssetCard Kukri
kukri = asset Kukri Cards.kukri

instance HasAbilities Kukri where
  getAbilities (Kukri a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage Kukri where
  runMessage msg a@(Kukri attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #combat 1)
      pushM $ mkChooseFight sid iid source
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTest \sid -> do
        actionRemainingCount <- field InvestigatorRemainingActions iid
        when (actionRemainingCount > 0) do
          chooseOneM iid do
            labeled "Spend 1 action to deal +1 damage" do
              push $ LoseActions iid (attrs.ability 1) 1
              skillTestModifier sid attrs iid (DamageDealt 1)
            labeled "Skip additional Kukri damage" nothing
      pure a
    _ -> Kukri <$> liftRunMessage msg attrs
