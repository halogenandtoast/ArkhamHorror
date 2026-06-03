module Arkham.Location.Cards.UndergroundPools (undergroundPools) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLostSister.Helpers

newtype UndergroundPools = UndergroundPools LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundPools :: LocationCard UndergroundPools
undergroundPools = locationWith UndergroundPools Cards.undergroundPools 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities UndergroundPools where
  getAbilities (UndergroundPools a) =
    scenarioI18n
      $ extendRevealed
        a
        [ withI18nTooltip "undergroundPools.remember"
            $ restricted a 1 (Here <> not_ (Remembered FoundASetOfFootprints))
            $ actionAbilityWithCost
            $ GroupClueCost (PerPlayer 2) (be a)
        , restricted a 2 Here
            $ forced
            $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
        ]

instance RunMessage UndergroundPools where
  runMessage msg l@(UndergroundPools attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember FoundASetOfFootprints
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure l
    _ -> UndergroundPools <$> liftRunMessage msg attrs
