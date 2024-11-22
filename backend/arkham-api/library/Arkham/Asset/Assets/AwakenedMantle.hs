module Arkham.Asset.Assets.AwakenedMantle (awakenedMantle, AwakenedMantle (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype AwakenedMantle = AwakenedMantle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakenedMantle :: AssetCard AwakenedMantle
awakenedMantle = assetWith AwakenedMantle Cards.awakenedMantle (healthL ?~ 2)

instance HasAbilities AwakenedMantle where
  getAbilities (AwakenedMantle x) =
    [ restricted
        x
        1
        (ControlsThis <> CanMoveTo (ConnectedFrom (YourLocation <> FloodedLocation) <> FloodedLocation))
        $ FastAbility (exhaust x)
    ]

instance RunMessage AwakenedMantle where
  runMessage msg a@(AwakenedMantle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        select
          $ CanMoveToLocation (InvestigatorWithId iid) (attrs.ability 1)
          $ ConnectedFrom (YourLocation <> FloodedLocation)
          <> FloodedLocation
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure a
    _ -> AwakenedMantle <$> liftRunMessage msg attrs
