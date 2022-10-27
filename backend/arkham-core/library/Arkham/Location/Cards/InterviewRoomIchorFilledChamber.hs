module Arkham.Location.Cards.InterviewRoomIchorFilledChamber
  ( interviewRoomIchorFilledChamber
  , InterviewRoomIchorFilledChamber(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype InterviewRoomIchorFilledChamber = InterviewRoomIchorFilledChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

interviewRoomIchorFilledChamber :: LocationCard InterviewRoomIchorFilledChamber
interviewRoomIchorFilledChamber = location
  InterviewRoomIchorFilledChamber
  Cards.interviewRoomIchorFilledChamber
  1
  (PerPlayer 1)

instance HasAbilities InterviewRoomIchorFilledChamber where
  getAbilities (InterviewRoomIchorFilledChamber attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage InterviewRoomIchorFilledChamber where
  runMessage msg (InterviewRoomIchorFilledChamber attrs) =
    InterviewRoomIchorFilledChamber <$> runMessage msg attrs
