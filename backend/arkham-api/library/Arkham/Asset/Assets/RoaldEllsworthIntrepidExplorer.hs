module Arkham.Asset.Assets.RoaldEllsworthIntrepidExplorer (
  roaldEllsworthIntrepidExplorer,
  RoaldEllsworthIntrepidExplorer (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype RoaldEllsworthIntrepidExplorer = RoaldEllsworthIntrepidExplorer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

roaldEllsworthIntrepidExplorer :: AssetCard RoaldEllsworthIntrepidExplorer
roaldEllsworthIntrepidExplorer = allyWith RoaldEllsworthIntrepidExplorer Cards.roaldEllsworthIntrepidExplorer (4, 2) noSlots

instance HasAbilities RoaldEllsworthIntrepidExplorer where
  getAbilities (RoaldEllsworthIntrepidExplorer a) =
    [ controlled a 1 (DuringTurn You <> exists (TreacheryAttachedToLocation Anywhere))
        $ FastAbility (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage RoaldEllsworthIntrepidExplorer where
  runMessage msg a@(RoaldEllsworthIntrepidExplorer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid (TreacheryAttachedToLocation Anywhere) \x -> do
        turnModifier iid (attrs.ability 1) x Blank
      pure a
    _ -> RoaldEllsworthIntrepidExplorer <$> liftRunMessage msg attrs
