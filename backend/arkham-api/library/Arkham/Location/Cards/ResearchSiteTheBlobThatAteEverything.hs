module Arkham.Location.Cards.ResearchSiteTheBlobThatAteEverything (researchSiteTheBlobThatAteEverything) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Token qualified as Token

newtype ResearchSiteTheBlobThatAteEverything = ResearchSiteTheBlobThatAteEverything LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchSiteTheBlobThatAteEverything :: LocationCard ResearchSiteTheBlobThatAteEverything
researchSiteTheBlobThatAteEverything = locationWith ResearchSiteTheBlobThatAteEverything Cards.researchSiteTheBlobThatAteEverything 2 (Static 0) connectsToAdjacent

instance HasAbilities ResearchSiteTheBlobThatAteEverything where
  getAbilities (ResearchSiteTheBlobThatAteEverything a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "researchSite.gainCountermeasure"
          $ restricted a 1 Here
          $ actionAbilityWithCost (clueCost 2)
      , scenarioI18n
          $ withI18nTooltip "researchSite.placeClues"
          $ restricted a 2 Here
          $ actionAbilityWithCost (SpendTokenCost Token.Resource (TargetIs ScenarioTarget))
      ]

instance RunMessage ResearchSiteTheBlobThatAteEverything where
  runMessage msg l@(ResearchSiteTheBlobThatAteEverything attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) ScenarioTarget #resource 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs #clue 2
      pure l
    _ -> ResearchSiteTheBlobThatAteEverything <$> liftRunMessage msg attrs
