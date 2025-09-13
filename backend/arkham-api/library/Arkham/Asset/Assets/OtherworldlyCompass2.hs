module Arkham.Asset.Assets.OtherworldlyCompass2 (otherworldlyCompass2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Prelude

newtype OtherworldlyCompass2 = OtherworldlyCompass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyCompass2 :: AssetCard OtherworldlyCompass2
otherworldlyCompass2 = asset OtherworldlyCompass2 Cards.otherworldlyCompass2

instance HasAbilities OtherworldlyCompass2 where
  getAbilities (OtherworldlyCompass2 a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage OtherworldlyCompass2 where
  runMessage msg a@(OtherworldlyCompass2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      revealedLocations <- selectCount $ RevealedLocation <> connectedTo (locationWithInvestigator iid)
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)
      enabled <- skillTestModifier sid attrs lid (ShroudModifier (-revealedLocations))
      pushAll [enabled, toMessage investigation]
      pure a
    _ -> OtherworldlyCompass2 <$> runMessage msg attrs
