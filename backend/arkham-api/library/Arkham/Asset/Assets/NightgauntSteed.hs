module Arkham.Asset.Assets.NightgauntSteed (nightgauntSteed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype NightgauntSteed = NightgauntSteed AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightgauntSteed :: AssetCard NightgauntSteed
nightgauntSteed = assetWith NightgauntSteed Cards.nightgauntSteed (healthL ?~ 3)

instance HasModifiersFor NightgauntSteed where
  getModifiersFor (NightgauntSteed a) = controllerGets a [CanEnterEmptySpace]

instance HasAbilities NightgauntSteed where
  getAbilities (NightgauntSteed a) =
    [ controlled_ a 1
        $ SilentForcedAbility
        $ Enters #after You (IncludeEmptySpace $ locationIs Locations.emptySpace)
    , controlled_ a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage NightgauntSteed where
  runMessage msg a@(NightgauntSteed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      dealAssetDamage attrs.id attrs 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      locations <-
        select
          $ IncludeEmptySpace
          $ NearestLocationTo iid (not_ (locationIs Locations.emptySpace) <> RevealedLocation)
      chooseTargetM iid locations $ moveTo (attrs.ability 2) iid
      pure a
    _ -> NightgauntSteed <$> liftRunMessage msg attrs
