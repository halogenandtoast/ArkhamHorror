module Arkham.Asset.Assets.JoeSargentRattletrapBusDriver (
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
import Arkham.Message.Lifted.Move

newtype JoeSargentRattletrapBusDriver = JoeSargentRattletrapBusDriver AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Story assets do not enter play in the same way so we hard code the uses for now
joeSargentRattletrapBusDriver :: AssetCard JoeSargentRattletrapBusDriver
joeSargentRattletrapBusDriver =
  allyWith
    JoeSargentRattletrapBusDriver
    Cards.joeSargentRattletrapBusDriver
    (1, 2)
    (tokensL . at Ticket ?~ 3)

instance HasAbilities JoeSargentRattletrapBusDriver where
  getAbilities (JoeSargentRattletrapBusDriver x) =
    [ mkAbility x 1 $ forced $ AssetLeavesPlay #when (be x)
    , restricted
        x
        2
        (ControlsThis <> exists (UnbarricadedConnectedFrom YourLocation <> not_ FullyFloodedLocation))
        $ FastAbility
        $ assetUseCost x Ticket 1
    ]

instance RunMessage JoeSargentRattletrapBusDriver where
  runMessage msg a@(JoeSargentRattletrapBusDriver attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <-
        select (UnbarricadedConnectedFrom (locationWithInvestigator iid) <> not_ FullyFloodedLocation)
      chooseTargetM iid locations $ moveTo (attrs.ability 2) iid
      pure a
    _ -> JoeSargentRattletrapBusDriver <$> liftRunMessage msg attrs
