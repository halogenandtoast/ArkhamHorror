module Arkham.Location.Cards.HallsOfPnakotusNorthernCorridors (hallsOfPnakotusNorthernCorridors) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype HallsOfPnakotusNorthernCorridors = HallsOfPnakotusNorthernCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusNorthernCorridors
  :: LocationCard HallsOfPnakotusNorthernCorridors
hallsOfPnakotusNorthernCorridors =
  setLabel "hallsOfPnakotusNorthernCorridors"
    $ location HallsOfPnakotusNorthernCorridors Cards.hallsOfPnakotusNorthernCorridors 3 (Static 1)

instance HasAbilities HallsOfPnakotusNorthernCorridors where
  getAbilities (HallsOfPnakotusNorthernCorridors a) =
    extendRevealed1 a $ restricted a 1 (Here <> DuringTurn You) $ FastAbility $ HandDiscardCost 1 #any

instance RunMessage HallsOfPnakotusNorthernCorridors where
  runMessage msg l@(HallsOfPnakotusNorthernCorridors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherHalls <- select $ LocationWithTitle "Halls of Pnakotus" <> not_ (be attrs)
      chooseTargetM iid otherHalls $ moveTo (attrs.ability 1) iid
      pure l
    _ -> HallsOfPnakotusNorthernCorridors <$> liftRunMessage msg attrs
