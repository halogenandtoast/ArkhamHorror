module Arkham.Campaign.Campaigns.TheCircleUndone (theCircleUndone) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.CampaignSteps
import Arkham.Campaigns.TheCircleUndone.Import
import Arkham.Campaigns.TheCircleUndone.Memento.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Decklist
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.Query
import Arkham.Helpers.Xp (XpBonus (WithBonus))
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Name (toTitle)
import Arkham.Question (Question (..))
import Arkham.Trait (Trait (SilverTwilight))

newtype TheCircleUndone = TheCircleUndone CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

instance IsCampaign TheCircleUndone where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just DisappearanceAtTheTwilightEstate
    DisappearanceAtTheTwilightEstate -> Just TheWitchingHour
    TheWitchingHour -> Just (UpgradeDeckStep AtDeathsDoorstep)
    AtDeathsDoorstep -> Just (UpgradeDeckStep TheSecretName)
    InterludeStep 2 _ -> Just (UpgradeDeckStep TheSecretName)
    TheSecretName -> Just (UpgradeDeckStep TheWagesOfSin)
    TheWagesOfSin -> Just (UpgradeDeckStep ForTheGreaterGood)
    ForTheGreaterGood -> Just (UpgradeDeckStep UnionAndDisillusion)
    InterludeStep 3 _ -> Just (UpgradeDeckStep UnionAndDisillusion)
    UnionAndDisillusion -> Just (UpgradeDeckStep InTheClutchesOfChaos)
    InTheClutchesOfChaos -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
    InterludeStep 4 _ -> Just (UpgradeDeckStep BeforeTheBlackThrone)
    BeforeTheBlackThrone -> Nothing
    EpilogueStep -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

theCircleUndone :: Difficulty -> TheCircleUndone
theCircleUndone difficulty =
  campaignWith
    TheCircleUndone
    (CampaignId "05")
    "The Circle Undone"
    difficulty
    (chaosBagContents difficulty)
    $ logL
    .~ mkCampaignLog
      { campaignLogRecordedSets =
          singletonMap (toCampaignLogKey MissingPersons)
            $ map (recorded . cdCardCode) allPrologueInvestigators
      }

allPrologueInvestigators :: [CardDef]
allPrologueInvestigators =
  [ Investigators.gavriellaMizrah
  , Investigators.jeromeDavids
  , Investigators.pennyWhite
  , Investigators.valentinoRivas
  ]

instance RunMessage TheCircleUndone where
  runMessage msg c@(TheCircleUndone attrs) = runQueueT $ case msg of
    StartCampaign | attrs.step `elem` [PrologueStep, DisappearanceAtTheTwilightEstate] -> do
      -- skip picking decks
      lead <- getActivePlayer
      unless (attrs.step `elem` [PrologueStep, DisappearanceAtTheTwilightEstate]) do
        push $ Ask lead PickCampaignSettings
      campaignStep_ $ if attrs.step == DisappearanceAtTheTwilightEstate then PrologueStep else attrs.step
      pure c
    CampaignStep PrologueStep -> do
      story prologue
      eachInvestigator (`forInvestigator` msg)
      story intro
      prologueStepPart 2
      nextCampaignStep
      pure c
    ForInvestigator iid (CampaignStep PrologueStep) -> do
      taken <- select Anyone
      let
        availablePrologueInvestigators =
          filter
            ((`notElem` taken) . InvestigatorId . cdCardCode)
            allPrologueInvestigators
      player <- getPlayer iid
      chooseOneM iid do
        questionLabeled
          "Choose one of the following neutral investigators to control for the duration of this prologue"
        for_ availablePrologueInvestigators \card -> do
          cardLabeled card do
            push
              $ LoadDecklist player
              $ ArkhamDBDecklist
                mempty
                mempty
                (InvestigatorId $ cdCardCode card)
                (toTitle card)
                Nothing
                Nothing
                Nothing -- TODO: should we figure out the taboo list here??
      pure c
    CampaignStep (PrologueStepPart 2) -> do
      taken <- selectMap unInvestigatorId Anyone
      let
        prologueInvestigatorsNotTaken =
          map cdCardCode allPrologueInvestigators
            \\ toList taken
        readingFor = \case
          "05046" -> gavriellaIntro
          "05047" -> jeromeIntro
          "05048" -> valentinoIntro
          "05049" -> pennyIntro
          _ -> error "Invalid prologue investigator"
        readings = map readingFor taken
      crossOutRecordSetEntries MissingPersons prologueInvestigatorsNotTaken
      traverse_ story readings
      pure c
    CampaignStep (InterludeStep 2 mInterludeKey) -> do
      anySilverTwilight <- selectAny $ InvestigatorWithTrait SilverTwilight
      let
        showThePriceOfProgress4 = mInterludeKey == Just ThePriceOfProgress4
        showThePriceOfProgress5 = mInterludeKey == Just ThePriceOfProgress5
        showThePriceOfProgress6 = mInterludeKey == Just ThePriceOfProgress6
        lodgeChoices = do
          labeled "\"I refuse to be part of this\"" $ interludeStepPart 2 mInterludeKey 7
          labeled "\"I agree\"" $ interludeStepPart 2 mInterludeKey 8
          labeled "\"I agree\" (You are lying)" $ interludeStepPart 2 mInterludeKey 9

      story $ if anySilverTwilight then thePriceOfProgress1 else thePriceOfProgress2
      story thePriceOfProgress3

      when showThePriceOfProgress4 do
        story thePriceOfProgress4
        record JosefDisappearedIntoTheMist
        record TheInvestigatorsAreEnemiesOfTheLodge
        nextCampaignStep

      when showThePriceOfProgress5 do
        record TheInvestigatorsRescuedJosef
        interludeXpAll (WithBonus "Gained insight into the inner workings of the Silver Twilight Lodge" 2)
        storyWithChooseOneM thePriceOfProgress5 lodgeChoices

      when showThePriceOfProgress6 do
        record JosefIsAliveAndWell
        storyWithChooseOneM thePriceOfProgress6 lodgeChoices
      pure c
    CampaignStep (InterludeStepPart 2 _ 7) -> do
      record TheInvestigatorsAreEnemiesOfTheLodge
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 2 _ 8) -> do
      record TheInvestigatorsAreMembersOfTheLodge
      addChaosToken Cultist
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 2 _ 9) -> do
      record TheInvestigatorsAreMembersOfTheLodge
      addChaosToken Cultist
      record TheInvestigatorsAreDeceivingTheLodge
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 3 mInterludeKey) -> do
      lead <- getLead
      story theInnerCircle1
      chooseOneM lead do
        labeled "Give Mr. Sanford everything you have found." $ interludeStepPart 3 mInterludeKey 2
        labeled "Tell him you have nothing to show. (You are lying.)" $ interludeStepPart 3 mInterludeKey 3
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 2) -> do
      rescuedJosef <- getHasRecord TheInvestigatorsRescuedJosef
      toldLodgeAboutCoven <- getHasRecord TheInvestigatorsToldTheLodgeAboutTheCoven
      someMementos <- getRecordSet MementosDiscovered
      let mementos = mapMaybe (unrecorded @Memento) someMementos
      story theInnerCircle2
      crossOutRecordSetEntries MementosDiscovered (toList mementos)
      interludeStepPart 3 mInterludeKey $ if rescuedJosef && toldLodgeAboutCoven then 4 else 5
      pure c
    CampaignStep (InterludeStepPart 3 _ 3) -> do
      story theInnerCircle3
      record TheInvestigatorsKeptsTheirMementosHidden
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 4) -> do
      story theInnerCircle4
      lead <- getLead
      chooseUpToNM lead 3 "Done asking question" do
        labeled "What is the creature?" $ story whatIsTheCreature
        labeled "What do you want with the creature?" $ story whatDoYouWantWithTheCreature
        labeled "What do the witches want with the creature?" $ story whatDoTheWitchesWantWithTheCreature
        labeled "Did you know about the creature before the charity gala?"
          $ story didYouKnowAboutTheCreatureBeforeTheCharityGala
        labeled "Where are the four missing people from the charity gala?"
          $ story whereAreTheFourMissingPeopleFromTheCharityGala
      interludeStepPart 3 mInterludeKey 6
      pure c
    CampaignStep (InterludeStepPart 3 _ 5) -> do
      story theInnerCircle5
      nextCampaignStep
      pure c
    CampaignStep (InterludeStepPart 3 _ 6) -> do
      story theInnerCircle6
      record TheInvestigatorsWereInductedIntoTheInnerCircle
      nextCampaignStep
      pure c
    CampaignStep (InterludeStep 4 _) -> do
      acceptedYourFate <- getHasRecord YouHaveAcceptedYourFate
      askedAnetteForAssistance <- getHasRecord TheInvestigatorsAskedAnetteForAssistance
      askedSanfordForAssistance <- getHasRecord TheInvestigatorsAskedSanfordForAssistance
      mementosDiscovered <- getMementosDiscoveredCount
      doomDrawsEverCloser <- getHasRecord DoomDrawsEverCloser
      hasBlackBook <- isJust <$> getOwner Assets.theBlackBook
      let
        total =
          getSum
            $ mconcat
              [ mwhen acceptedYourFate (Sum 1)
              , mwhen askedAnetteForAssistance (Sum 2)
              , mwhen askedSanfordForAssistance (Sum 2)
              , mwhen (mementosDiscovered >= 3) (Sum 1)
              , mwhen (mementosDiscovered >= 6) (Sum 1)
              , mwhen (mementosDiscovered >= 9) (Sum 1)
              , mwhen hasBlackBook (Sum 1)
              , mwhen doomDrawsEverCloser (Sum 2)
              ]

      story twistOfFate1
      recordCount ThePathWindsBeforeYou total
      when (askedAnetteForAssistance || askedSanfordForAssistance) do
        addChaosToken $ fromDifficulty MinusThree MinusFour MinusFive MinusSix attrs.difficulty
      story twistOfFate2
      nextCampaignStep
      pure c
    CampaignStep EpilogueStep -> do
      whenHasRecord TheInvestigatorsArrestedAnette $ story epilogueArrestedAnette
      whenHasRecord TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
        $ story epilogueAssumedControlOfTheLodge
      whenHasRecord TheInvestigatorsSurvivedTheWatchersEmbrace $ story epilogueSurvivedTheWatchersEmbrace
      whenHasRecord TheInvestigatorsSignedTheBlackBookOfAzathoth $ story epilogueSignedTheBlackBook
      gameOver

      pure c
    HandleOption option -> do
      investigators <- allInvestigators
      case option of
        TakeBlackBook -> forceAddCampaignCardToDeckChoice investigators Assets.theBlackBook
        TakePuzzleBox -> forceAddCampaignCardToDeckChoice investigators Assets.puzzleBox
        ProceedToInterlude3 -> pure ()
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> lift $ defaultCampaignRunner msg c
