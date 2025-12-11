module Arkham.Location.Cards.RamblingRouteB (ramblingRouteB) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher.Patterns
import Arkham.Scenarios.WithoutATrace.Helpers

newtype RamblingRouteB = RamblingRouteB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteB :: LocationCard RamblingRouteB
ramblingRouteB = symbolLabel $ locationWith RamblingRouteB Cards.ramblingRouteB 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RamblingRouteB where
  getAbilities (RamblingRouteB a) =
    extendRevealed a []

instance RunMessage RamblingRouteB where
  runMessage msg l@(RamblingRouteB attrs) = runQueueT $ case msg of
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
    _ -> RamblingRouteB <$> liftRunMessage msg attrs
