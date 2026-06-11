module Arkham.Act.Cards.DistortionsInTimeGroupA (distortionsInTimeGroupA) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.FlavorText
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype DistortionsInTimeGroupA = DistortionsInTimeGroupA ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Timed - Do not advance this act until you are instructed. When the agenda
-- advances, there must be at least 11 doom in play.
distortionsInTimeGroupA :: ActCard DistortionsInTimeGroupA
distortionsInTimeGroupA = act (2, A) DistortionsInTimeGroupA Cards.distortionsInTimeGroupA Nothing

instance RunMessage DistortionsInTimeGroupA where
  runMessage msg a@(DistortionsInTimeGroupA attrs) = runQueueT $ scenarioI18n $ scope "distortionsInTime" $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      doom <- getDoomCount
      if doom >= 11
        then do
          flavor $ h "title" >> p "success"
          push $ ScenarioSpecific "act3Setup" Null
          advanceActDeck attrs
        else do
          flavor $ h "title" >> p "failure"
          push R1
      pure a
    _ -> DistortionsInTimeGroupA <$> liftRunMessage msg attrs
