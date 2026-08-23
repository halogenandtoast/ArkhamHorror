module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.BackAlleyDawn (backAlleyDawn) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype BackAlleyDawn = BackAlleyDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backAlleyDawn :: LocationCard BackAlleyDawn
backAlleyDawn =
  symbolLabel $ location BackAlleyDawn Cards.backAlleyDawn 3 (PerPlayer 2)

instance HasAbilities BackAlleyDawn where
  getAbilities (BackAlleyDawn a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> NoCluesOnThis
            <> not_ (thisIs a $ LocationWithCardsUnderneath AnyCards)
        )
        (actionAbilityWithCost $ AddTokenCost 1 #blood)

instance RunMessage BackAlleyDawn where
  runMessage msg l@(BackAlleyDawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \i -> do
        ls <- getConnectedMoveLocations i (attrs.ability 1)
        chooseTargetM i ls $ moveTo (attrs.ability 1) i
      connected <- select $ ConnectedTo ForMovement (be attrs)
      selectEach (enemyAt attrs) \e -> chooseTargetM iid connected $ enemyMoveTo (attrs.ability 1) e
      removeLocation attrs
      pure l
    _ -> BackAlleyDawn <$> liftRunMessage msg attrs
