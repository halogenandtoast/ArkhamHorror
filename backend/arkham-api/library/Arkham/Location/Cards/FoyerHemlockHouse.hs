module Arkham.Location.Cards.FoyerHemlockHouse (foyerHemlockHouse) where

import Arkham.Ability hiding (resignAction)
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype FoyerHemlockHouse = FoyerHemlockHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerHemlockHouse :: LocationCard FoyerHemlockHouse
foyerHemlockHouse = locationWith FoyerHemlockHouse Cards.foyerHemlockHouse 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities FoyerHemlockHouse where
  getAbilities (FoyerHemlockHouse a) =
    extendRevealed
      a
      [ playerLimit PerTurn
          $ restricted a 1 (Here <> DuringTurn You <> CanMoveTo (ConnectedFrom ForMovement (be a)))
          $ FastAbility Free
      , resignAction a
      ]

instance RunMessage FoyerHemlockHouse where
  runMessage msg l@(FoyerHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      choices <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseTargetM iid choices $ moveTo (attrs.ability 1) iid
      pure l
    _ -> FoyerHemlockHouse <$> liftRunMessage msg attrs
