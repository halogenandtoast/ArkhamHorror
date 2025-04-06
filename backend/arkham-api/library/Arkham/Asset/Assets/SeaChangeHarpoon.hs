module Arkham.Asset.Assets.SeaChangeHarpoon (seaChangeHarpoon) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SeaChangeHarpoon = SeaChangeHarpoon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaChangeHarpoon :: AssetCard SeaChangeHarpoon
seaChangeHarpoon = asset SeaChangeHarpoon Cards.seaChangeHarpoon

instance HasAbilities SeaChangeHarpoon where
  getAbilities (SeaChangeHarpoon attrs) = [restricted attrs 1 ControlsThis fightAction_]

instance RunMessage SeaChangeHarpoon where
  runMessage msg a@(SeaChangeHarpoon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers
        sid
        (attrs.ability 1)
        iid
        [SkillModifier #combat 1, CriteriaModifier (exists $ CardIsCommittedBy (InvestigatorWithId iid) <> #skill) (DamageDealt 1)]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    SkillTestEnds sid iid (isAbilitySource attrs 1 -> True) -> do
      skills <- select $ skillControlledBy iid
      chooseOneM iid do
        labeled
          "Return Sea Change Harpoon to your hand to return all of your committed skill cards to your hand instead of discarding them"
          do
            returnToHand iid attrs
            for_ skills \s -> skillTestModifier sid (attrs.ability 1) s ReturnToHandAfterTest
        labeled "Do nothing" nothing
      pure a
    _ -> SeaChangeHarpoon <$> liftRunMessage msg attrs
