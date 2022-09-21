module Arkham.Treachery.Cards.TimelineDestabilization
  ( timelineDestabilization
  , TimelineDestabilization(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TimelineDestabilization = TimelineDestabilization TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelineDestabilization :: TreacheryCard TimelineDestabilization
timelineDestabilization =
  treachery TimelineDestabilization Cards.timelineDestabilization

instance RunMessage TimelineDestabilization where
  runMessage msg t@(TimelineDestabilization attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> TimelineDestabilization <$> runMessage msg attrs
