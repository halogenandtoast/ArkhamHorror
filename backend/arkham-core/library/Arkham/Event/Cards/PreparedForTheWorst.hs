module Arkham.Event.Cards.PreparedForTheWorst (preparedForTheWorst, PreparedForTheWorst (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype PreparedForTheWorst = PreparedForTheWorst EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preparedForTheWorst :: EventCard PreparedForTheWorst
preparedForTheWorst = event PreparedForTheWorst Cards.preparedForTheWorst

instance RunMessage PreparedForTheWorst where
  runMessage msg e@(PreparedForTheWorst attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ search iid attrs iid [fromTopOfDeck 9] (basic $ #asset <> #weapon) (DrawFound iid 1)
      pure e
    _ -> PreparedForTheWorst <$> runMessage msg attrs
