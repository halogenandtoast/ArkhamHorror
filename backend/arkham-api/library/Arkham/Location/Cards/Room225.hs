module Arkham.Location.Cards.Room225 (
  room225,
  Room225 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MurderAtTheExcelsiorHotel.Helpers

newtype Room225 = Room225 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room225 :: LocationCard Room225
room225 = locationWith Room225 Cards.room225 3 (PerPlayer 1) (labelL .~ "room225")

instance HasAbilities Room225 where
  getAbilities (Room225 attrs) =
    scenarioI18n
      $ withRevealedAbilities
        attrs
      $ [ skillTestAbility
          $ withI18nTooltip "room225.cleanBlood"
          $ restrictedAbility attrs 1 Here actionAbility
      , skillTestAbility
          $ withI18nTooltip "room225.hideBody"
          $ restrictedAbility attrs 2 Here actionAbility
      , skillTestAbility
          $ withI18nTooltip "room225.tidyRoom"
          $ restrictedAbility attrs 3 Here actionAbility
      ]

instance RunMessage Room225 where
  runMessage msg l@(Room225 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (toAbilitySource attrs 1) iid #willpower (Fixed 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (toAbilitySource attrs 2) iid #combat (Fixed 3)
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (toAbilitySource attrs 3) iid #intellect (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ Remember CleanedUpTheBlood
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      push $ Remember HidTheBody
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 3 -> True) -> do
      push $ Remember TidiedUpTheRoom
      pure l
    _ -> Room225 <$> runMessage msg attrs
