module Arkham.Treachery.Cards.RexsCurseSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Calculation
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ gameTest $ \investigator -> do
    rexsCurse <- genPlayerCard Cards.rexsCurse

    pushAndRunAll [loadDeck investigator [rexsCurse], drawCards (toId investigator) investigator 1]
    assert
      $ selectAny
        ( TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
            <> treacheryIs Cards.rexsCurse
        )

  it "causes you to reveal another token" $ gameTest $ \investigator -> do
    updateInvestigator investigator (intellectL .~ 5)
    rexsCurse <- genPlayerCard Cards.rexsCurse

    didRunMessage <- didPassSkillTestBy investigator SkillIntellect 1

    pushAndRunAll
      [ SetChaosTokens [PlusOne]
      , loadDeck investigator [rexsCurse]
      , drawCards (toId investigator) investigator 1
      , BeginSkillTest
          $ initSkillTest
            (toId investigator)
            (TestSource mempty)
            TestTarget
            SkillIntellect
            (SkillTestDifficulty $ Fixed 5)
      ]
    chooseOnlyOption "start skill test"
    chooseOnlyOption "trigger rex's curse"
    chooseOnlyOption "apply results"
    assert
      $ selectAny
        ( TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
            <> treacheryIs Cards.rexsCurse
        )
    didRunMessage `refShouldBe` True

  it "is shuffled back into your deck if you fail the test" $ gameTest $ \investigator -> do
    updateInvestigator investigator (intellectL .~ 5)
    rexsCurse <- genPlayerCard Cards.rexsCurse
    pushAndRunAll
      [ SetChaosTokens [MinusOne]
      , loadDeck investigator [rexsCurse]
      , drawCards (toId investigator) investigator 1
      , beginSkillTest investigator SkillIntellect 4
      ]
    chooseOnlyOption "start skill test"
    -- we sneak in this modifier to cause the next test (with the same token) to fail instead
    pushAndRun
      $ skillTestModifier (TestSource mempty) (toTarget investigator) (SkillModifier SkillIntellect (-1))
    chooseOnlyOption "trigger rex's curse"
    chooseOnlyOption "apply results"
    assert
      $ selectNone
      $ TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
      <> treacheryIs Cards.rexsCurse
    fieldAssert InvestigatorDeck ((== 1) . length) investigator
