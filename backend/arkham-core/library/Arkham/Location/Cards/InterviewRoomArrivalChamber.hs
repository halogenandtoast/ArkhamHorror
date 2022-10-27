module Arkham.Location.Cards.InterviewRoomArrivalChamber
  ( interviewRoomArrivalChamber
  , InterviewRoomArrivalChamber(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype InterviewRoomArrivalChamber = InterviewRoomArrivalChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomArrivalChamber :: LocationCard InterviewRoomArrivalChamber
interviewRoomArrivalChamber = location
  InterviewRoomArrivalChamber
  Cards.interviewRoomArrivalChamber
  2
  (Static 0)

instance HasAbilities InterviewRoomArrivalChamber where
  getAbilities (InterviewRoomArrivalChamber attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage InterviewRoomArrivalChamber where
  runMessage msg (InterviewRoomArrivalChamber attrs) =
    InterviewRoomArrivalChamber <$> runMessage msg attrs
