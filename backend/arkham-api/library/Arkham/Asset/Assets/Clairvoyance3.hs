module Arkham.Asset.Assets.Clairvoyance3 (clairvoyance3, Clairvoyance3 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier

newtype Clairvoyance3 = Clairvoyance3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance3 :: AssetCard Clairvoyance3
clairvoyance3 = asset Clairvoyance3 Cards.clairvoyance3

instance HasAbilities Clairvoyance3 where
  getAbilities (Clairvoyance3 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage Clairvoyance3 where
  runMessage msg a@(Clairvoyance3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid attrs iid [DiscoveredClues 1, SkillModifier #willpower 2]
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #plusone, #zero]) attrs attrs do
        assignHorror iid source 1
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> Clairvoyance3 <$> liftRunMessage msg attrs
