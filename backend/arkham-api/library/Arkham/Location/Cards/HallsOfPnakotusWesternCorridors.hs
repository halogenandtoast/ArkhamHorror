module Arkham.Location.Cards.HallsOfPnakotusWesternCorridors (hallsOfPnakotusWesternCorridors) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype HallsOfPnakotusWesternCorridors = HallsOfPnakotusWesternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusWesternCorridors
  :: LocationCard HallsOfPnakotusWesternCorridors
hallsOfPnakotusWesternCorridors =
  setLabel "hallsOfPnakotusWesternCorridors"
    $ location HallsOfPnakotusWesternCorridors Cards.hallsOfPnakotusWesternCorridors 3 (Static 1)

instance HasAbilities HallsOfPnakotusWesternCorridors where
  getAbilities (HallsOfPnakotusWesternCorridors a) =
    extendRevealed1 a $ restricted a 1 (Here <> DuringTurn You) $ FastAbility $ HandDiscardCost 1 #any

instance RunMessage HallsOfPnakotusWesternCorridors where
  runMessage msg l@(HallsOfPnakotusWesternCorridors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherHalls <- select $ LocationWithTitle "Halls of Pnakotus" <> not_ (be attrs)
      chooseTargetM iid otherHalls $ moveTo (attrs.ability 1) iid
      pure l
    _ -> HallsOfPnakotusWesternCorridors <$> liftRunMessage msg attrs
