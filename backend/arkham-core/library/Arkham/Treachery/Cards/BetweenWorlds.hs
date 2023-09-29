module Arkham.Treachery.Cards.BetweenWorlds (
  betweenWorlds,
  BetweenWorlds (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Label
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (Discarded)
import Arkham.Movement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BetweenWorlds = BetweenWorlds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

betweenWorlds :: TreacheryCard BetweenWorlds
betweenWorlds = treachery BetweenWorlds Cards.betweenWorlds

instance RunMessage BetweenWorlds where
  runMessage msg t@(BetweenWorlds attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      let
        asLocation =
          lookupEncounterCard Locations.betweenWorlds (toCardId attrs)
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      useLabel2 <- selectAny $ LocationWithLabel $ mkLabel "betweenWorlds1"
      locationId <- getRandom
      pushAll
        [ PlaceLocation locationId (EncounterCard asLocation)
        , SetLocationLabel
            locationId
            (if useLabel2 then "betweenWorlds2" else "betweenWorlds1")
        , AddDirectConnection locationId nexus
        , AddDirectConnection nexus locationId
        , MoveTo $ move (toSource attrs) iid locationId
        ]
      pure t
    After (Revelation _ source) | isSource attrs source -> do
      push (Discarded (toTarget attrs) (toSource attrs) (toCard attrs)) -- Using discarded to remove existence)
      pure t
    _ -> BetweenWorlds <$> runMessage msg attrs
