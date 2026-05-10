module Arkham.Asset.Assets.AnyuFaithfulCompanion (anyuFaithfulCompanion) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers (campaignI18n)
import Arkham.Helpers.CombatTarget
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype AnyuFaithfulCompanion = AnyuFaithfulCompanion AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anyuFaithfulCompanion :: AssetCard AnyuFaithfulCompanion
anyuFaithfulCompanion = ally AnyuFaithfulCompanion Cards.anyuFaithfulCompanion (2, 2)

instance HasAbilities AnyuFaithfulCompanion where
  getAbilities (AnyuFaithfulCompanion a) = [restricted a 1 ControlsThis $ FastAbility (exhaust a)]

anyuChoices :: ReverseQueue m => AssetAttrs -> InvestigatorId -> ChooseT m ()
anyuChoices attrs iid = do
  locations <- getAccessibleLocations iid (attrs.ability 1)
  unless (null locations) do
    labeledI "moveToConnecting" do
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
  (campaignI18n $ labeled' "anyuFaithfulCompanion.gain2SkillValue") do
    nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue 2)
  canEvade <- hasEvadeTargets (attrs.ability 1) iid
  when canEvade do
    (campaignI18n $ labeled' "anyuFaithfulCompanion.attemptToEvadeWithABaseAgilitySkillOf4") do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #agility 4)
      chooseEvadeEnemy sid iid (attrs.ability 1)

instance RunMessage AnyuFaithfulCompanion where
  runMessage msg a@(AnyuFaithfulCompanion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        anyuChoices attrs iid
        (campaignI18n $ labeled' "anyuFaithfulCompanion.discardAnyuToChooseUpToThreeInAnyOrder") do
          toDiscardBy iid (attrs.ability 1) attrs
          chooseUpToNM iid 3 "Done choosing options" $ anyuChoices attrs iid
      pure a
    _ -> AnyuFaithfulCompanion <$> liftRunMessage msg attrs
