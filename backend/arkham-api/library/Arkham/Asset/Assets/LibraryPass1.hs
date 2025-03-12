module Arkham.Asset.Assets.LibraryPass1 (libraryPass1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype LibraryPass1 = LibraryPass1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryPass1 :: AssetCard LibraryPass1
libraryPass1 = asset LibraryPass1 Cards.libraryPass1

instance HasAbilities LibraryPass1 where
  getAbilities (LibraryPass1 x) =
    [ controlled
        x
        1
        ( DuringTurn You
            <> not_ (exists $ AssetAttachedToAsset (be x))
            <> exists (InHandOf NotForPlay You <> basic (#tome <> #asset))
        )
        $ FastAbility (ResourceCost 1)
    , restricted x 2 (ControlsThis <> exists (AssetAttachedToAsset (be x))) $ forced $ TurnEnds #when You
    ]

instance RunMessage LibraryPass1 where
  runMessage msg a@(LibraryPass1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <- select $ inHandOf NotForPlay iid <> basic (#tome <> #asset)
      focusCards cards do
        assetId <- getRandom
        chooseTargetM iid cards \card ->
          push $ CreateAssetAt assetId card $ AttachedToAsset attrs.id (Just $ InPlayArea iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      select (AssetAttachedToAsset $ be attrs) >>= traverse_ (putOnBottomOfDeck iid iid)
      pure a
    _ -> LibraryPass1 <$> liftRunMessage msg attrs
