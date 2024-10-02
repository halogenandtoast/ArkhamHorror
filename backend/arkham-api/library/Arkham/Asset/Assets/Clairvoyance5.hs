module Arkham.Asset.Assets.Clairvoyance5 (clairvoyance5, Clairvoyance5 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier

newtype Clairvoyance5 = Clairvoyance5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance5 :: AssetCard Clairvoyance5
clairvoyance5 = asset Clairvoyance5 Cards.clairvoyance5

instance HasAbilities Clairvoyance5 where
  getAbilities (Clairvoyance5 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage Clairvoyance5 where
  runMessage msg a@(Clairvoyance5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #plusone, #zero]) attrs attrs do
        assignHorror iid source 2
      skillTestModifiers sid attrs iid [DiscoveredClues 2, SkillModifier #willpower 3]
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> Clairvoyance5 <$> liftRunMessage msg attrs
