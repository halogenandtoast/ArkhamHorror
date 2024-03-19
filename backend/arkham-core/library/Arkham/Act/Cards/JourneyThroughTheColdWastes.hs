module Arkham.Act.Cards.JourneyThroughTheColdWastes
  ( JourneyThroughTheColdWastes(..)
  , journeyThroughTheColdWastes
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype JourneyThroughTheColdWastes = JourneyThroughTheColdWastes ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

journeyThroughTheColdWastes :: ActCard JourneyThroughTheColdWastes
journeyThroughTheColdWastes = act (1, A) JourneyThroughTheColdWastes Cards.journeyThroughTheColdWastes (groupClueCost $ PerPlayer 2)
 
instance RunMessage JourneyThroughTheColdWastes where
  runMessage msg a@(JourneyThroughTheColdWastes attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> JourneyThroughTheColdWastes <$> lift (runMessage msg attrs)
