module Arkham.Act.Cards.DistortionsInTimeGroupA (distortionsInTimeGroupA) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype DistortionsInTimeGroupA = DistortionsInTimeGroupA ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

distortionsInTimeGroupA :: ActCard DistortionsInTimeGroupA
distortionsInTimeGroupA = act (2, A) DistortionsInTimeGroupA Cards.distortionsInTimeGroupA Nothing

instance RunMessage DistortionsInTimeGroupA where
  runMessage msg a@(DistortionsInTimeGroupA attrs) = runQueueT $ case msg of
    ScenarioSpecific "doomThresholdMet" _ -> do
      pure $ DistortionsInTimeGroupA $ attrs & setMeta True
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      if toResultDefault False attrs.meta
        then do
          scenarioSpecific_ "act3Setup"
          advanceActDeck attrs
        else push R1
      pure a
    _ -> DistortionsInTimeGroupA <$> liftRunMessage msg attrs
