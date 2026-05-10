module Arkham.Treachery.Cards.DreamlandsEclipseSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Ability.Types (abilitySource)
import Arkham.Classes.HasGame (getGame)
import Arkham.GameEnv (getJustSkillTest)
import Arkham.Helpers.SkillTest (getModifiedSkillTestDifficulty)
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Dreamlands Eclipse" $ do
  context "forced ability during an Ancient Stone investigation" $ do
    it "applies +2 shroud to the investigated location" . gameTest $ \self -> do
      ancientStone <- self `putAssetIntoPlay` Assets.ancientStone1
      dreamlandsEclipse <- self `putTreacheryIntoPlay` Treacheries.dreamlandsEclipse
      location <- testLocation & prop @"shroud" 4 & prop @"clues" 1
      self `moveTo` location

      duringTurn self do
        [investigateAction] <- self `getActionsFrom` ancientStone
        self `useAbility` investigateAction
        useDreamlandsEclipseForcedAbility dreamlandsEclipse
        chooseLabel "$theDreamEaters.theSearchForKadath.dreamlandsEclipse.label.shroudPlus2"

        skillTest <- getJustSkillTest
        difficulty <- getModifiedSkillTestDifficulty skillTest
        difficulty `shouldBe` 9

    it "leaves the difficulty unchanged when taking 1 horror instead" . gameTest $ \self -> do
      ancientStone <- self `putAssetIntoPlay` Assets.ancientStone1
      dreamlandsEclipse <- self `putTreacheryIntoPlay` Treacheries.dreamlandsEclipse
      location <- testLocation & prop @"shroud" 4 & prop @"clues" 1
      self `moveTo` location

      duringTurn self do
        [investigateAction] <- self `getActionsFrom` ancientStone
        self `useAbility` investigateAction
        useDreamlandsEclipseForcedAbility dreamlandsEclipse
        chooseLabel "$label.takeHorror"

        skillTest <- getJustSkillTest
        difficulty <- getModifiedSkillTestDifficulty skillTest
        difficulty `shouldBe` 7

useDreamlandsEclipseForcedAbility :: HasCallStack => TreacheryId -> TestAppT ()
useDreamlandsEclipseForcedAbility dreamlandsEclipse = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      QuestionLabel _ _ q -> go q
      q -> go q
    other ->
      liftIO
        $ expectationFailure
        $ "expected exactly one question while choosing Dreamlands Eclipse forced ability, got: "
        <> show other
 where
  go = \case
    ChooseOne msgs -> chooseFrom msgs
    PlayerWindowChooseOne msgs -> chooseFrom msgs
    ChooseOneAtATime msgs -> chooseFrom msgs
    ChooseN _ msgs -> chooseFrom msgs
    question ->
      liftIO
        $ expectationFailure
        $ "unsupported question while choosing Dreamlands Eclipse forced ability: "
        <> show question

  chooseFrom msgs = case find matchAbility msgs of
    Just msg -> push (uiToRun msg) >> runMessages
    Nothing ->
      liftIO
        $ expectationFailure
        $ "could not find Dreamlands Eclipse forced ability in messages: "
        <> show msgs

  matchAbility = \case
    AbilityLabel {ability} -> abilitySource ability == toSource dreamlandsEclipse
    _ -> False

chooseLabel :: HasCallStack => Text -> TestAppT ()
chooseLabel expected = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(_, question)] -> case question of
      QuestionLabel _ _ q -> go q
      q -> go q
    other ->
      liftIO
        $ expectationFailure
        $ "expected exactly one question while choosing label "
        <> show expected
        <> ", got: "
        <> show other
 where
  go = \case
    ChooseOne msgs -> chooseFrom msgs
    PlayerWindowChooseOne msgs -> chooseFrom msgs
    ChooseOneAtATime msgs -> chooseFrom msgs
    ChooseN _ msgs -> chooseFrom msgs
    question ->
      liftIO
        $ expectationFailure
        $ "unsupported question while choosing label "
        <> show expected
        <> ": "
        <> show question

  chooseFrom msgs = case find matchLabel msgs of
    Just msg -> push (uiToRun msg) >> runMessages
    Nothing ->
      liftIO
        $ expectationFailure
        $ "could not find label "
        <> show expected
        <> " in messages: "
        <> show msgs

  matchLabel = \case
    Label {label} -> label == expected
    _ -> False
