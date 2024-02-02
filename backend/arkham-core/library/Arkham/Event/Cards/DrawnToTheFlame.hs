module Arkham.Event.Cards.DrawnToTheFlame where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

drawnToTheFlame :: EventCard DrawnToTheFlame
drawnToTheFlame = event DrawnToTheFlame Cards.drawnToTheFlame

instance RunMessage DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      pushAll
        [ InvestigatorDrawEncounterCard iid
        , toMessage $ discoverAtYourLocation iid attrs 2
        ]
      pure e
    _ -> DrawnToTheFlame <$> runMessage msg attrs
