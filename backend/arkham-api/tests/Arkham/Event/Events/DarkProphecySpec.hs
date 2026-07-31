module Arkham.Event.Events.DarkProphecySpec (spec) where

import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.Event.Cards qualified as Events
import TestImport.New

-- | The face of each chaos token group currently offered as something to resolve.
offeredGroupFaces :: HasCallStack => TestAppT [ChaosTokenFace]
offeredGroupFaces = do
  questionMap <- gameQuestion <$> getGame
  choices <- case map snd (mapToList questionMap) of
    [question] -> case stripQuestionWrappers question of
      ChooseOne msgs -> pure msgs
      other -> [] <$ expectationFailure ("expected a ChooseOne, got: " <> show other)
    other -> [] <$ expectationFailure ("expected exactly one question, got: " <> show other)
  pure
    [ chaosTokenFace token
    | ChaosTokenGroupChoice _ _ (ChooseMatch _ _ _ _ tokens _ _) <- choices
    , token <- concat tokens
    ]

spec :: Spec
spec = describe "Dark Prophecy" $ do
  it "makes you resolve a symbol token when one is revealed (#5317)" . gameTest $ \self -> do
    withProp @"resources" 1 self
    withProp @"intellect" 5 self
    darkProphecy <- genCard Events.darkProphecy
    self `addToHand` darkProphecy
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
    self `moveTo` location
    setChaosTokens [Skull, Zero, MinusOne, MinusTwo, MinusThree]
    self `investigate` location
    startSkillTest

    -- Before the fix this threw "This matcher can not be nested": the
    -- ChooseMatch step matches each revealed token against
    -- 'ChaosTokenMatchesOrElse', and the caller wraps it in 'IncludeSealed'.
    chooseTarget darkProphecy

    offeredGroupFaces `shouldReturn` [Skull]

  it "lets you resolve any of the five when no symbol is revealed" . gameTest $ \self -> do
    withProp @"resources" 1 self
    withProp @"intellect" 5 self
    darkProphecy <- genCard Events.darkProphecy
    self `addToHand` darkProphecy
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
    self `moveTo` location
    setChaosTokens [PlusOne, Zero, MinusOne, MinusTwo, MinusThree]
    self `investigate` location
    startSkillTest
    chooseTarget darkProphecy

    -- The 'orElse' branch of the matcher: nothing matches the preferred
    -- symbol-token side, so every group stays on offer.
    faces <- offeredGroupFaces
    sort faces `shouldBe` sort [PlusOne, Zero, MinusOne, MinusTwo, MinusThree]
