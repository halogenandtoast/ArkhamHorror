module Arkham.Location.Cards.OutsidersLairWithoutATrace (outsidersLairWithoutATrace) where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WithoutATrace.Helpers

newtype OutsidersLairWithoutATrace = OutsidersLairWithoutATrace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outsidersLairWithoutATrace :: LocationCard OutsidersLairWithoutATrace
outsidersLairWithoutATrace =
  symbolLabel
    $ locationWith
      OutsidersLairWithoutATrace
      Cards.outsidersLairWithoutATrace
      5
      (PerPlayer 2)
      connectsToAdjacent

instance HasAbilities OutsidersLairWithoutATrace where
  getAbilities (OutsidersLairWithoutATrace a) =
    extendRevealed a []

instance RunMessage OutsidersLairWithoutATrace where
  runMessage msg l@(OutsidersLairWithoutATrace attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> do
            cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
            unless (null cards) do
              exposedInShadows iid attrs $ chooseTargetM iid cards $ hollow iid
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    _ -> OutsidersLairWithoutATrace <$> liftRunMessage msg attrs
