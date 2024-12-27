module Arkham.Asset.Assets.AnyuFaithfulCompanion (anyuFaithfulCompanion) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Window
import Arkham.Helpers.Modifiers (ModifierType (..), effectModifiers)
import Arkham.Matcher
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
  locations <- select $ AccessibleFrom (locationWithInvestigator iid)
  unless (null locations) do
    labeled "Move to a connecting location" do
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
  labeled "You get +2 skill value for your next skill test this round" do
    ems <- effectModifiers (attrs.ability 1) [AnySkillValue 2]
    push
      $ CreateWindowModifierEffect
        (FirstEffectWindow [EffectNextSkillTestWindow, EffectRoundWindow])
        ems
        (attrs.ability 1)
        (toTarget iid)
  canEvade <- selectAny $ CanEvadeEnemy (attrs.ability 1)
  when canEvade do
    labeled "_Evade_. Attempt to evade with a base {agility} skill of 4" do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #agility 4)
      chooseEvadeEnemy sid iid (attrs.ability 1)

instance RunMessage AnyuFaithfulCompanion where
  runMessage msg a@(AnyuFaithfulCompanion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        anyuChoices attrs iid
        labeled "Discard Anyu to choose up to three, in any order" do
          toDiscardBy iid (attrs.ability 1) attrs
          chooseUpToNM iid 3 "Done choosing options" $ anyuChoices attrs iid
      pure a
    _ -> AnyuFaithfulCompanion <$> liftRunMessage msg attrs
