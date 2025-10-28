module Arkham.Location.Cards.ReturnToSouthChurch (returnToSouthChurch) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype ReturnToSouthChurch = ReturnToSouthChurch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSouthChurch :: LocationCard ReturnToSouthChurch
returnToSouthChurch = location ReturnToSouthChurch Cards.returnToSouthChurch 3 (Static 0)

instance HasAbilities ReturnToSouthChurch where
  getAbilities (ReturnToSouthChurch a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> thisExists a (LocationWithBreaches $ atLeast 1)) doubleActionAbility
      , scenarioI18n $ withI18nTooltip "southChurch.resign" $ locationResignAction a
      ]

instance RunMessage ReturnToSouthChurch where
  runMessage msg l@(ReturnToSouthChurch attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      let n = countLocationBreaches attrs
      act <- selectJust AnyAct
      removeBreaches attrs n
      placeBreaches act n
      pure l
    _ -> ReturnToSouthChurch <$> liftRunMessage msg attrs
