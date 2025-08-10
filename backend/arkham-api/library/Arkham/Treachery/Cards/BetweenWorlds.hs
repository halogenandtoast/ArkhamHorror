module Arkham.Treachery.Cards.BetweenWorlds (betweenWorlds) where

import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Helpers.Location
import Arkham.Label
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (Discarded)
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BetweenWorlds = BetweenWorlds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

betweenWorlds :: TreacheryCard BetweenWorlds
betweenWorlds = treachery BetweenWorlds Cards.betweenWorlds

instance RunMessage BetweenWorlds where
  runMessage msg t@(BetweenWorlds attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let asLocation = lookupEncounterCard Locations.betweenWorlds (toCardId attrs)
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      useLabel2 <- selectAny $ LocationWithLabel $ mkLabel "betweenWorlds1"

      locationId <- placeLocation (EncounterCard asLocation)
      setLocationLabel locationId $ if useLabel2 then "betweenWorlds2" else "betweenWorlds1"
      connectBothWays locationId nexus
      moveTo attrs iid locationId
      pure t
    After (Revelation _ (isSource attrs -> True)) -> do
      -- Using discarded to remove existence)
      push $ Discarded (toTarget attrs) (toSource attrs) (toCard attrs)
      pure t
    _ -> BetweenWorlds <$> liftRunMessage msg attrs
