module Arkham.Location.Cards.InterviewRoomRestrainingChamber
  ( interviewRoomRestrainingChamber
  , InterviewRoomRestrainingChamber(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype InterviewRoomRestrainingChamber = InterviewRoomRestrainingChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomRestrainingChamber :: LocationCard InterviewRoomRestrainingChamber
interviewRoomRestrainingChamber = location
  InterviewRoomRestrainingChamber
  Cards.interviewRoomRestrainingChamber
  4
  (PerPlayer 1)

instance HasAbilities InterviewRoomRestrainingChamber where
  getAbilities (InterviewRoomRestrainingChamber attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage InterviewRoomRestrainingChamber where
  runMessage msg (InterviewRoomRestrainingChamber attrs) =
    InterviewRoomRestrainingChamber <$> runMessage msg attrs
