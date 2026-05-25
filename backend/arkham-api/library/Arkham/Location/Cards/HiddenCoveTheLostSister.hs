module Arkham.Location.Cards.HiddenCoveTheLostSister (hiddenCoveTheLostSister) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLostSister.Helpers

newtype HiddenCoveTheLostSister = HiddenCoveTheLostSister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenCoveTheLostSister :: LocationCard HiddenCoveTheLostSister
hiddenCoveTheLostSister = locationWith HiddenCoveTheLostSister Cards.hiddenCoveTheLostSister 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities HiddenCoveTheLostSister where
  getAbilities (HiddenCoveTheLostSister a) =
    scenarioI18n
      $ extendRevealed
        a
        [ withI18nTooltip "hiddenCove.remember"
            $ restricted a 1 (Here <> not_ (Remembered FoundATornDogLeash))
            $ actionAbilityWithCost
            $ GroupClueCost (PerPlayer 2) (be a)
        , restricted a 2 Here
            $ forced
            $ SkillTestResult #after You (WhileAttackingAnEnemy AnyEnemy) #failure
        ]

instance RunMessage HiddenCoveTheLostSister where
  runMessage msg l@(HiddenCoveTheLostSister attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember FoundATornDogLeash
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    _ -> HiddenCoveTheLostSister <$> liftRunMessage msg attrs
