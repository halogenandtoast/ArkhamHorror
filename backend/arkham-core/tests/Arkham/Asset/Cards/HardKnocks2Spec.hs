module Arkham.Asset.Cards.HardKnocks2Spec (
  spec,
) where

import TestImport

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Token

spec :: Spec
spec = describe "Hard Knocks (2)" $ do
  it "Adds 1 to combat check for each resource spent"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorCombat = 1, investigatorTokens = setTokens Resource 2 mempty}
      putCardIntoPlay investigator Assets.hardKnocks2
      didPassTest <- didPassSkillTestBy investigator SkillCombat 0
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ beginSkillTest investigator SkillCombat 3
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

  it "Adds 1 to agility check for each resource spent"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorAgility = 1, investigatorTokens = setTokens Resource 2 mempty}
      putCardIntoPlay investigator Assets.hardKnocks2

      didPassTest <- didPassSkillTestBy investigator SkillAgility 0

      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ beginSkillTest investigator SkillAgility 3
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
