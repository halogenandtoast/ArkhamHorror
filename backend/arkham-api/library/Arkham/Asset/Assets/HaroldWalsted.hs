module Arkham.Asset.Assets.HaroldWalsted (haroldWalsted) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers (controllerGetsMaybe, ModifierType (..))
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Trait

newtype HaroldWalsted = HaroldWalsted AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haroldWalsted :: AssetCard HaroldWalsted
haroldWalsted = allyWith HaroldWalsted Cards.haroldWalsted (1, 1) noSlots

instance HasAbilities HaroldWalsted where
  getAbilities (HaroldWalsted x) = [mkAbility x 1 $ forced $ AssetLeavesPlay #when (be x)]

instance HasModifiersFor HaroldWalsted where
  getModifiersFor (HaroldWalsted a) = controllerGetsMaybe a \iid -> do
    ensure $ isSkillTestInvestigator iid
    ensure $ isInvestigationOf $ LocationWithTrait Miskatonic
    pure [SkillModifier #intellect 2]

instance RunMessage HaroldWalsted where
  runMessage msg a@(HaroldWalsted attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addChaosToken Tablet
      removeFromGame attrs
      pure a
    _ -> HaroldWalsted <$> liftRunMessage msg attrs
