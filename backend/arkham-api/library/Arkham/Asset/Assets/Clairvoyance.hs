module Arkham.Asset.Assets.Clairvoyance (clairvoyance, Clairvoyance (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier

newtype Clairvoyance = Clairvoyance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clairvoyance :: AssetCard Clairvoyance
clairvoyance = asset Clairvoyance Cards.clairvoyance

instance HasAbilities Clairvoyance where
  getAbilities (Clairvoyance a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage Clairvoyance where
  runMessage msg a@(Clairvoyance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid attrs iid (DiscoveredClues 1)
      onRevealChaosTokenEffect sid (oneOf [#eldersign, #plusone, #zero]) attrs attrs do
        assignHorror iid source 1
      aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    _ -> Clairvoyance <$> liftRunMessage msg attrs
