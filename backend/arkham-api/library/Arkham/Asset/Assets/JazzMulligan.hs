module Arkham.Asset.Assets.JazzMulligan (jazzMulligan) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Trait

newtype JazzMulligan = JazzMulligan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jazzMulligan :: AssetCard JazzMulligan
jazzMulligan = allyWith JazzMulligan Cards.jazzMulligan (2, 2) noSlots

instance HasAbilities JazzMulligan where
  getAbilities (JazzMulligan x) =
    [skillTestAbility $ restricted x 1 (Uncontrolled <> OnSameLocation) parleyAction_]

instance HasModifiersFor JazzMulligan where
  getModifiersFor (JazzMulligan a) = for_ a.controller \iid -> do
    active <- iid <=~> ActiveInvestigator
    modifySelectWhen a active UnrevealedLocation [TraitRestrictedModifier Miskatonic Blank]

instance RunMessage JazzMulligan where
  runMessage msg a@(JazzMulligan attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid (place attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfAsset iid attrs
      pure a
    _ -> JazzMulligan <$> liftRunMessage msg attrs
