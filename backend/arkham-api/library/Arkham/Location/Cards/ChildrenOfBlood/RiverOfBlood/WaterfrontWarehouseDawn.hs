module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterfrontWarehouseDawn (waterfrontWarehouseDawn) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype WaterfrontWarehouseDawn = WaterfrontWarehouseDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterfrontWarehouseDawn :: LocationCard WaterfrontWarehouseDawn
waterfrontWarehouseDawn =
  symbolLabel $ location WaterfrontWarehouseDawn Cards.waterfrontWarehouseDawn 5 (PerPlayer 1)

instance HasAbilities WaterfrontWarehouseDawn where
  getAbilities (WaterfrontWarehouseDawn a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> NoCluesOnThis
            <> not_ (thisIs a $ LocationWithCardsUnderneath AnyCards)
        )
        (actionAbilityWithCost $ AddTokenCost 1 #blood)

instance RunMessage WaterfrontWarehouseDawn where
  runMessage msg l@(WaterfrontWarehouseDawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \i -> do
        ls <- getConnectedMoveLocations i (attrs.ability 1)
        chooseTargetM i ls $ moveTo (attrs.ability 1) i
      connected <- select $ ConnectedTo ForMovement (be attrs)
      selectEach (enemyAt attrs) \e -> chooseTargetM iid connected $ enemyMoveTo (attrs.ability 1) e
      removeLocation attrs
      pure l
    _ -> WaterfrontWarehouseDawn <$> liftRunMessage msg attrs
