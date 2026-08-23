module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterfrontWarehouseDusk (waterfrontWarehouseDusk) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype WaterfrontWarehouseDusk = WaterfrontWarehouseDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterfrontWarehouseDusk :: LocationCard WaterfrontWarehouseDusk
waterfrontWarehouseDusk =
  symbolLabel $ location WaterfrontWarehouseDusk Cards.waterfrontWarehouseDusk 5 (PerPlayer 1)

instance HasAbilities WaterfrontWarehouseDusk where
  getAbilities (WaterfrontWarehouseDusk a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> NoCluesOnThis
            <> not_ (thisIs a $ LocationWithCardsUnderneath AnyCards)
        )
        (actionAbilityWithCost $ AddTokenCost 2 #blood)

instance RunMessage WaterfrontWarehouseDusk where
  runMessage msg l@(WaterfrontWarehouseDusk attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \i -> do
        ls <- getConnectedMoveLocations i (attrs.ability 1)
        chooseTargetM i ls $ moveTo (attrs.ability 1) i
      connected <- select $ ConnectedTo ForMovement (be attrs)
      selectEach (enemyAt attrs) \e -> chooseTargetM iid connected $ enemyMoveTo (attrs.ability 1) e
      removeLocation attrs
      pure l
    _ -> WaterfrontWarehouseDusk <$> liftRunMessage msg attrs
