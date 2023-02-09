module Arkham.Event.Cards.LookWhatIFound where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Message

newtype LookWhatIFound = LookWhatIFound EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound :: EventCard LookWhatIFound
lookWhatIFound = event LookWhatIFound Cards.lookWhatIFound

instance RunMessage LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getJustLocation iid
      e
        <$ pushAll
             [ InvestigatorDiscoverClues iid lid 2 Nothing
             , discard attrs
             ]
    _ -> LookWhatIFound <$> runMessage msg attrs
