module Arkham.Asset.Cards.OtherworldlyCompass2 (
  otherworldlyCompass2,
  OtherworldlyCompass2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

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
      revealedLocations <- selectCount $ RevealedLocation <> ConnectedTo (locationWithInvestigator iid)
      investigation <- mkInvestigate iid (toAbilitySource attrs 1)
      pushAll
        [ skillTestModifier attrs lid (ShroudModifier (-revealedLocations))
        , toMessage investigation
        ]
      pure a
    _ -> OtherworldlyCompass2 <$> runMessage msg attrs
