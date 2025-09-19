module Arkham.Location.Cards.HallsOfPnakotusSouthernCorridors (hallsOfPnakotusSouthernCorridors) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype HallsOfPnakotusSouthernCorridors = HallsOfPnakotusSouthernCorridors LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallsOfPnakotusSouthernCorridors :: LocationCard HallsOfPnakotusSouthernCorridors
hallsOfPnakotusSouthernCorridors =
  setLabel "hallsOfPnakotusSouthernCorridors"
    $ location
      HallsOfPnakotusSouthernCorridors
      Cards.hallsOfPnakotusSouthernCorridors
      3
      (Static 1)

instance HasModifiersFor HallsOfPnakotusSouthernCorridors where
  getModifiersFor (HallsOfPnakotusSouthernCorridors a) = do
    modifySelect
      a
      (locationIs Cards.hallsOfPnakotusEasternCorridors)
      [ConnectedToWhen (locationIs Cards.hallsOfPnakotusEasternCorridors) (LocationWithId a.id)]
    modifySelect
      a
      (locationIs Cards.hallsOfPnakotusWesternCorridors)
      [ConnectedToWhen (locationIs Cards.hallsOfPnakotusWesternCorridors) (LocationWithId a.id)]

instance HasAbilities HallsOfPnakotusSouthernCorridors where
  getAbilities (HallsOfPnakotusSouthernCorridors a) =
    extendRevealed1 a $ restricted a 1 (Here <> DuringTurn You) $ FastAbility $ HandDiscardCost 1 #any

instance RunMessage HallsOfPnakotusSouthernCorridors where
  runMessage msg l@(HallsOfPnakotusSouthernCorridors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherHalls <- select $ LocationWithTitle "Halls of Pnakotus" <> not_ (be attrs)
      chooseTargetM iid otherHalls $ moveTo (attrs.ability 1) iid
      pure l
    _ -> HallsOfPnakotusSouthernCorridors <$> liftRunMessage msg attrs
