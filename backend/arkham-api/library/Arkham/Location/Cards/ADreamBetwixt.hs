module Arkham.Location.Cards.ADreamBetwixt (aDreamBetwixt) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Stories

newtype ADreamBetwixt = ADreamBetwixt LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aDreamBetwixt :: LocationCard ADreamBetwixt
aDreamBetwixt = locationWith ADreamBetwixt Cards.aDreamBetwixt 4 (Static 0) (canBeFlippedL .~ True)

instance HasAbilities ADreamBetwixt where
  getAbilities (ADreamBetwixt a) =
    extendRevealed
      a
      [ restricted a 1 (HasScenarioCount StrengthOfTheAbyss $ EqualTo (Static 0))
          $ forced
          $ ScenarioCountDecremented #after StrengthOfTheAbyss
      , restricted a 2 (thisExists a LocationCanBeFlipped)
          $ freeReaction
          $ SkillTestResult #after You (WhileInvestigating $ be a) (SuccessResult AnyValue)
      ]

instance RunMessage ADreamBetwixt where
  runMessage msg l@(ADreamBetwixt attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addStrengthOfTheAbyss 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.toTheDreamlands
      pure . ADreamBetwixt $ attrs & canBeFlippedL .~ False
    _ -> ADreamBetwixt <$> liftRunMessage msg attrs
