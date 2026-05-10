module Arkham.Location.Cards.RailroadStation (railroadStation, RailroadStation (..)) where

import Arkham.Ability hiding (resignAction)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype RailroadStation = RailroadStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

railroadStation :: LocationCard RailroadStation
railroadStation = locationWith RailroadStation Cards.railroadStation 1 (Static 0) connectsToAdjacent

instance HasAbilities RailroadStation where
  getAbilities (RailroadStation attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs <> FloodedLocation)
      , scenarioI18n $ withI18nTooltip "railroadStation.resign" $ resignAction attrs
      ]

instance RunMessage RailroadStation where
  runMessage msg l@(RailroadStation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> RailroadStation <$> liftRunMessage msg attrs
