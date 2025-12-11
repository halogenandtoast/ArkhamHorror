module Arkham.Location.Cards.RamblingRouteA (ramblingRouteA) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WithoutATrace.Helpers

newtype RamblingRouteA = RamblingRouteA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteA :: LocationCard RamblingRouteA
ramblingRouteA = symbolLabel $ locationWith RamblingRouteA Cards.ramblingRouteA 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities RamblingRouteA where
  getAbilities (RamblingRouteA a) =
    extendRevealed a []

instance RunMessage RamblingRouteA where
  runMessage msg l@(RamblingRouteA attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> pure ()
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    _ -> RamblingRouteA <$> liftRunMessage msg attrs
