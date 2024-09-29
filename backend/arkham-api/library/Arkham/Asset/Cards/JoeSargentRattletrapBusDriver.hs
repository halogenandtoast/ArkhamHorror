module Arkham.Asset.Cards.JoeSargentRattletrapBusDriver (
  joeSargentRattletrapBusDriver,
  JoeSargentRattletrapBusDriver (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype JoeSargentRattletrapBusDriver = JoeSargentRattletrapBusDriver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeSargentRattletrapBusDriver :: AssetCard JoeSargentRattletrapBusDriver
joeSargentRattletrapBusDriver = asset JoeSargentRattletrapBusDriver Cards.joeSargentRattletrapBusDriver

instance HasAbilities JoeSargentRattletrapBusDriver where
  getAbilities (JoeSargentRattletrapBusDriver x) =
    [ mkAbility x 1 $ forced $ AssetLeavesPlay #when (be x)
    , restricted x 2 (ControlsThis <> exists (ConnectedFrom YourLocation <> not_ FullyFloodedLocation))
        $ FastAbility
        $ assetUseCost x Ticket 1
    ]

instance RunMessage JoeSargentRattletrapBusDriver where
  runMessage msg a@(JoeSargentRattletrapBusDriver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- select $ ConnectedFrom (locationWithInvestigator iid) <> not_ FullyFloodedLocation
      chooseTargetM iid locations $ moveTo (attrs.ability 2) iid
      pure a
    _ -> JoeSargentRattletrapBusDriver <$> liftRunMessage msg attrs
