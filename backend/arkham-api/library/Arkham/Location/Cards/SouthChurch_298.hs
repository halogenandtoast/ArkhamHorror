module Arkham.Location.Cards.SouthChurch_298 (southChurch_298) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype SouthChurch_298 = SouthChurch_298 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch_298 :: LocationCard SouthChurch_298
southChurch_298 = location SouthChurch_298 Cards.southChurch_298 1 (Static 0)

instance HasAbilities SouthChurch_298 where
  getAbilities (SouthChurch_298 a) =
    let n = countLocationBreaches a
     in extendRevealed
          a
          [ restricted a 1 (Here <> (if n > 0 then EncounterDeckIsNotEmpty else Never)) actionAbility
          , scenarioI18n $ withI18nTooltip "southChurch.resign" $ locationResignAction a
          ]

instance RunMessage SouthChurch_298 where
  runMessage msg l@(SouthChurch_298 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let breachCount = countLocationBreaches attrs
      act <- selectJust AnyAct
      drawEncounterCard iid attrs
      removeBreaches attrs breachCount
      placeBreaches act breachCount
      pure l
    _ -> SouthChurch_298 <$> liftRunMessage msg attrs
