module Arkham.Event.Events.DarkInsight (darkInsight) where

import Arkham.Script
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (cancelCardDraw)

newtype DarkInsight = DarkInsight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkInsight :: EventCard DarkInsight
darkInsight = event DarkInsight Cards.darkInsight

instance RunMessage DarkInsight where
  runMessage = script $ onPlay $ cancelCardDraw >> shuffleDrawnCardBackIntoDeck
