module Arkham.Location.Cards.AlienFrontierA (alienFrontierA) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WithoutATrace.Helpers

newtype AlienFrontierA = AlienFrontierA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFrontierA :: LocationCard AlienFrontierA
alienFrontierA = symbolLabel $ locationWith AlienFrontierA Cards.alienFrontierA 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities AlienFrontierA where
  getAbilities (AlienFrontierA a) =
    extendRevealed a []

instance RunMessage AlienFrontierA where
  runMessage msg l@(AlienFrontierA attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> pure ()
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    _ -> AlienFrontierA <$> liftRunMessage msg attrs
