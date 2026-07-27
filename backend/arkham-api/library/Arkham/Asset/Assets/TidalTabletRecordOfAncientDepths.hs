module Arkham.Asset.Assets.TidalTabletRecordOfAncientDepths (tidalTablet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (
  decreaseThisFloodLevel,
  increaseThisFloodLevel,
 )
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype TidalTabletRecordOfAncientDepths = TidalTabletRecordOfAncientDepths AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidalTablet :: AssetCard TidalTabletRecordOfAncientDepths
tidalTablet = asset TidalTabletRecordOfAncientDepths Cards.tidalTablet

instance HasModifiersFor TidalTabletRecordOfAncientDepths where
  getModifiersFor (TidalTabletRecordOfAncientDepths a) = artifactModifiers a

instance HasAbilities TidalTabletRecordOfAncientDepths where
  getAbilities (TidalTabletRecordOfAncientDepths a) =
    [ restricted a 1 ControlsThis
        $ triggered (FloodLevelIncreased #after YourLocation) Free
    , -- Only usable once the glyphs naming this ability have been translated.
      restricted a 2 (ControlsThis <> glyphsAllKnown "LKD") $ FastAbility $ exhaust a
    , artifactAbility a 3
    ]

instance RunMessage TidalTabletRecordOfAncientDepths where
  runMessage msg a@(TidalTabletRecordOfAncientDepths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lid <- getJustLocation iid
      increaseThisFloodLevel lid
      others <- select $ FloodedLocation <> not_ (LocationWithId lid)
      chooseTargetM iid others decreaseThisFloodLevel
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      handOffArtifact iid attrs
      pure a
    _ -> TidalTabletRecordOfAncientDepths <$> liftRunMessage msg attrs
