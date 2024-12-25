module Arkham.Asset.Assets.RoaldEllsworthIntrepidExplorerResolute (
  roaldEllsworthIntrepidExplorerResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype RoaldEllsworthIntrepidExplorerResolute = RoaldEllsworthIntrepidExplorerResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

roaldEllsworthIntrepidExplorerResolute :: AssetCard RoaldEllsworthIntrepidExplorerResolute
roaldEllsworthIntrepidExplorerResolute =
  allyWith
    RoaldEllsworthIntrepidExplorerResolute
    Cards.roaldEllsworthIntrepidExplorerResolute
    (5, 2)
    noSlots

instance HasAbilities RoaldEllsworthIntrepidExplorerResolute where
  getAbilities (RoaldEllsworthIntrepidExplorerResolute a) =
    [ controlled
        a
        1
        (DuringTurn You <> exists (TreacheryIsNonWeakness <> TreacheryAttachedToLocation Anywhere))
        $ FastAbility (OrCost [exhaust a, assetUseCost a Supply 1])
    ]

instance RunMessage RoaldEllsworthIntrepidExplorerResolute where
  runMessage msg a@(RoaldEllsworthIntrepidExplorerResolute attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid (TreacheryIsNonWeakness <> TreacheryAttachedToLocation Anywhere) \x -> do
        turnModifier iid (attrs.ability 1) x Blank
      pure a
    _ -> RoaldEllsworthIntrepidExplorerResolute <$> liftRunMessage msg attrs
