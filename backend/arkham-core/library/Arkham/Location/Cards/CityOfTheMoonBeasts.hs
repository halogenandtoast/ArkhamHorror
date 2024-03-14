module Arkham.Location.Cards.CityOfTheMoonBeasts (cityOfTheMoonBeasts, CityOfTheMoonBeasts (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (beginSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype CityOfTheMoonBeasts = CityOfTheMoonBeasts LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheMoonBeasts :: LocationCard CityOfTheMoonBeasts
cityOfTheMoonBeasts = location CityOfTheMoonBeasts Cards.cityOfTheMoonBeasts 0 (PerPlayer 1)

instance HasModifiersFor CityOfTheMoonBeasts where
  getModifiersFor target (CityOfTheMoonBeasts attrs) | attrs `is` target = do
    x <- getMaxAlarmLevel
    pure $ toModifiers attrs [ShroudModifier x]
  getModifiersFor _ _ = pure []

instance HasAbilities CityOfTheMoonBeasts where
  getAbilities (CityOfTheMoonBeasts attrs) =
    extendRevealed
      attrs
      [restrictedAbility attrs 1 (exists (investigatorAt attrs)) $ forced $ RoundEnds #when]

instance RunMessage CityOfTheMoonBeasts where
  runMessage msg l@(CityOfTheMoonBeasts attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator $ \iid -> beginSkillTest iid (attrs.ability 1) iid #agility 2
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      raiseAlarmLevel (attrs.ability 1) iid
      pure l
    _ -> CityOfTheMoonBeasts <$> lift (runMessage msg attrs)
