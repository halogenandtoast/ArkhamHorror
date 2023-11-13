module Arkham.Asset.Cards.ArcaneStudies2Spec (
  spec,
) where

import TestImport.Lifted

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Token

spec :: Spec
spec = describe "Arcane Studies (2)" $ do
  it "Adds 1 to willpower check for each resource spent" $ do
    gameTest $ \investigator -> do
      didPassTest <- didPassSkillTestBy investigator SkillWillpower 0
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorWillpower = 1, investigatorTokens = addTokens Resource 2 mempty}
      pushAndRun $ SetChaosTokens [Zero]
      putCardIntoPlay investigator Assets.arcaneStudies2
      pushAndRun $ beginSkillTest investigator SkillWillpower 3
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {ability} -> abilityIndex ability == 1
            _ -> False
        )
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {ability} -> abilityIndex ability == 1
            _ -> False
        )
      chooseOptionMatching
        "start skill test"
        ( \case
            StartSkillTestButton {} -> True
            _ -> False
        )
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True

  it "Adds 1 to intellect check for each resource spent" $ do
    gameTest $ \investigator -> do
      updateInvestigator investigator $ \attrs ->
        attrs {investigatorIntellect = 1, investigatorTokens = addTokens Resource 2 mempty}

      didPassTest <- didPassSkillTestBy investigator SkillIntellect 0
      pushAndRun $ SetChaosTokens [Zero]
      putCardIntoPlay investigator Assets.arcaneStudies2
      pushAndRun $ beginSkillTest investigator SkillIntellect 3
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {ability} -> abilityIndex ability == 2
            _ -> False
        )
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {ability} -> abilityIndex ability == 2
            _ -> False
        )
      chooseOptionMatching
        "start skill test"
        ( \case
            StartSkillTestButton {} -> True
            _ -> False
        )
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True
