module Arkham.Act.Cards.RaceForAnswers (raceForAnswers) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Matcher

newtype RaceForAnswers = RaceForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

raceForAnswers :: ActCard RaceForAnswers
raceForAnswers = act (1, A) RaceForAnswers Cards.raceForAnswers (groupClueCost (PerPlayer 2))

instance RunMessage RaceForAnswers where
  runMessage msg a@(RaceForAnswers attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      playerCount <- getPlayerCount
      selectEach (RevealedLocation <> "Historical Society") \location ->
        push $ PlaceCluesUpToClueValue location (toSource attrs) playerCount
      advanceActDeck attrs
      pure a
    _ -> RaceForAnswers <$> liftRunMessage msg attrs
