module Arkham.Location.Cards.AlienFrontierB (alienFrontierB) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher.Patterns
import Arkham.Scenarios.WithoutATrace.Helpers

newtype AlienFrontierB = AlienFrontierB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFrontierB :: LocationCard AlienFrontierB
alienFrontierB = symbolLabel $ locationWith AlienFrontierB Cards.alienFrontierB 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AlienFrontierB where
  getAbilities (AlienFrontierB a) =
    extendRevealed a []

instance RunMessage AlienFrontierB where
  runMessage msg l@(AlienFrontierB attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> do
            whenMatch iid InvestigatorWithAnyResources do
              exposedInShadows iid attrs $ loseResources iid (attrs.ability (-1)) 2
          MiddlePosition -> do
            whenMatch iid InvestigatorWithAnyActionsRemaining do
              exposedInShadows iid attrs $ loseActions iid (attrs.ability (-1)) 1
          RightPosition -> pure ()
      pure l
    _ -> AlienFrontierB <$> liftRunMessage msg attrs
