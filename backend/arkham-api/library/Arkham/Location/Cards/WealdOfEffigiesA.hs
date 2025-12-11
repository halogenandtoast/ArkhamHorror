module Arkham.Location.Cards.WealdOfEffigiesA (wealdOfEffigiesA) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher.Patterns
import Arkham.Scenarios.WithoutATrace.Helpers

newtype WealdOfEffigiesA = WealdOfEffigiesA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wealdOfEffigiesA :: LocationCard WealdOfEffigiesA
wealdOfEffigiesA =
  symbolLabel
    $ locationWith WealdOfEffigiesA Cards.wealdOfEffigiesA 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WealdOfEffigiesA where
  getAbilities (WealdOfEffigiesA a) =
    extendRevealed a []

instance RunMessage WealdOfEffigiesA where
  runMessage msg l@(WealdOfEffigiesA attrs) = runQueueT $ case msg of
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
    _ -> WealdOfEffigiesA <$> liftRunMessage msg attrs
