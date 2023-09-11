module Arkham.Event.Cards.LookWhatIFound where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Message

newtype LookWhatIFound = LookWhatIFound EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound :: EventCard LookWhatIFound
lookWhatIFound = event LookWhatIFound Cards.lookWhatIFound

instance RunMessage LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      push $ InvestigatorDiscoverClues iid lid (toSource attrs) 2 Nothing
      pure e
    _ -> LookWhatIFound <$> runMessage msg attrs
