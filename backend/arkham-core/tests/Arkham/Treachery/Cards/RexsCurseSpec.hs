module Arkham.Treachery.Cards.RexsCurseSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Investigator.Types
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ gameTest $ \investigator -> do
    rexsCurse <- genPlayerCard Cards.rexsCurse
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll [loadDeck investigator [rexsCurse], drawing]
    assert $
      selectAny
        ( TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
            <> treacheryIs Cards.rexsCurse
        )

  it "causes you to reveal another token" $ gameTest $ \investigator -> do
    updateInvestigator investigator (intellectL .~ 5)
    rexsCurse <- genPlayerCard Cards.rexsCurse
    drawing <- drawCards (toId investigator) investigator 1

    didRunMessage <- didPassSkillTestBy investigator SkillIntellect 1

    pushAndRunAll
      [ SetTokens [PlusOne]
      , loadDeck investigator [rexsCurse]
      , drawing
      , BeginSkillTest $
          initSkillTest
            (toId investigator)
            (TestSource mempty)
            TestTarget
            SkillIntellect
            5
      ]
    chooseOnlyOption "start skill test"
    chooseOnlyOption "trigger rex's curse"
    chooseOnlyOption "apply results"
    assert $
      selectAny
        ( TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
            <> treacheryIs Cards.rexsCurse
        )
    didRunMessage `refShouldBe` True

  it "is shuffled back into your deck if you fail the test" $ gameTest $ \investigator -> do
    updateInvestigator investigator (intellectL .~ 5)
    rexsCurse <- genPlayerCard Cards.rexsCurse
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ SetTokens [MinusOne]
      , loadDeck investigator [rexsCurse]
      , drawing
      , beginSkillTest investigator SkillIntellect 4
      ]
    chooseOnlyOption "start skill test"
    -- we sneak in this modifier to cause the next test (with the same token) to fail instead
    pushAndRun $
      skillTestModifier (TestSource mempty) (toTarget investigator) (SkillModifier SkillIntellect (-1))
    chooseOnlyOption "trigger rex's curse"
    chooseOnlyOption "apply results"
    assert $
      selectNone $
        TreacheryInThreatAreaOf (InvestigatorWithId $ toId investigator)
          <> treacheryIs Cards.rexsCurse
    fieldAssert InvestigatorDeck ((== 1) . length) investigator
