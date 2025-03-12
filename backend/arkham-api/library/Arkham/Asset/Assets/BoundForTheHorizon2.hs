module Arkham.Asset.Assets.BoundForTheHorizon2 (boundForTheHorizon2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype BoundForTheHorizon2 = BoundForTheHorizon2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boundForTheHorizon2 :: AssetCard BoundForTheHorizon2
boundForTheHorizon2 = asset BoundForTheHorizon2 Cards.boundForTheHorizon2

instance HasAbilities BoundForTheHorizon2 where
  getAbilities (BoundForTheHorizon2 x) =
    [ controlled x 1 (DuringTurn You <> youExist (InvestigatorCanMoveTo (x.ability 1) AccessibleLocation))
        $ FastAbility' (exhaust x <> HandDiscardCost 1 #any) [#move]
    ]

instance RunMessage BoundForTheHorizon2 where
  runMessage msg a@(BoundForTheHorizon2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connectingLocations <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid connectingLocations $ moveTo (attrs.ability 1) iid
      pure a
    _ -> BoundForTheHorizon2 <$> liftRunMessage msg attrs
