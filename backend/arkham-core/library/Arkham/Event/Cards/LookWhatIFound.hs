module Arkham.Event.Cards.LookWhatIFound where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator

newtype LookWhatIFound = LookWhatIFound EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lookWhatIFound :: EventCard LookWhatIFound
lookWhatIFound = event LookWhatIFound Cards.lookWhatIFound

instance RunMessage LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      lid <- getJustLocation iid
      push $ discover iid lid attrs 2
      pure e
    _ -> LookWhatIFound <$> runMessage msg attrs
