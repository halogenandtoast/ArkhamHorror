module Arkham.Location.Cards.HallsOfPnakotusEasternCorridors (hallsOfPnakotusEasternCorridors) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype HallsOfPnakotusEasternCorridors = HallsOfPnakotusEasternCorridors LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusEasternCorridors
  :: LocationCard HallsOfPnakotusEasternCorridors
hallsOfPnakotusEasternCorridors =
  setLabel "hallsOfPnakotusEasternCorridors"
    $ location HallsOfPnakotusEasternCorridors Cards.hallsOfPnakotusEasternCorridors 3 (Static 1)

instance HasAbilities HallsOfPnakotusEasternCorridors where
  getAbilities (HallsOfPnakotusEasternCorridors a) =
    extendRevealed1 a $ restricted a 1 (Here <> DuringTurn You) $ FastAbility $ HandDiscardCost 1 #any

instance RunMessage HallsOfPnakotusEasternCorridors where
  runMessage msg l@(HallsOfPnakotusEasternCorridors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherHalls <- select $ LocationWithTitle "Halls of Pnakotus" <> not_ (be attrs)
      chooseTargetM iid otherHalls $ moveTo (attrs.ability 1) iid
      pure l
    _ -> HallsOfPnakotusEasternCorridors <$> liftRunMessage msg attrs
