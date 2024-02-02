module Arkham.Campaign.Campaigns.TheCircleUndone (
  TheCircleUndone (..),
  theCircleUndone,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.Campaign.Runner
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheCircleUndone.Import
import Arkham.Card.CardDef
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Decklist
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait (Trait (SilverTwilight))

newtype TheCircleUndone = TheCircleUndone CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, NoThunks, NFData)

instance IsCampaign TheCircleUndone where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just DisappearanceAtTheTwilightEstate
    DisappearanceAtTheTwilightEstate -> Just TheWitchingHour
    TheWitchingHour -> Just (UpgradeDeckStep AtDeathsDoorstep)
    AtDeathsDoorstep -> Just (UpgradeDeckStep TheSecretName)
    InterludeStep 2 _ -> Just TheSecretName
    TheSecretName -> Just (UpgradeDeckStep TheWagesOfSin)
    TheWagesOfSin -> Just (UpgradeDeckStep ForTheGreaterGood)
    ForTheGreaterGood -> Just UnionAndDisillusion
    InterludeStep 3 _ -> Just UnionAndDisillusion
    UnionAndDisillusion -> Just (UpgradeDeckStep InTheClutchesOfChaos)
    InTheClutchesOfChaos -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
    InterludeStep 4 _ -> Just BeforeTheBlackThrone
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
          singletonMap MissingPersons
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
  runMessage msg c@(TheCircleUndone attrs) = case msg of
    StartCampaign -> do
      -- skip picking decks
      lead <- getActivePlayer
      pushAll
        $ [Ask lead PickCampaignSettings | campaignStep attrs /= PrologueStep]
        <> [CampaignStep $ campaignStep attrs]
      pure c
    CampaignStep PrologueStep -> do
      players <- allPlayers
      pushAll
        $ story players prologue
        : [ ForPlayer player (CampaignStep PrologueStep)
          | player <- players
          ]
          <> [ story players intro
             , CampaignStep (PrologueStepPart 2)
             , NextCampaignStep Nothing
             ]
      pure c
    ForPlayer player (CampaignStep PrologueStep) -> do
      taken <- selectList Anyone
      let
        availablePrologueInvestigators =
          filter
            ((`notElem` taken) . InvestigatorId . cdCardCode)
            allPrologueInvestigators
      push
        $ questionLabel
          "Choose one of the following neutral investigators to control for the duration of this prologue"
          player
        $ ChooseOne
          [ CardLabel
            (cdCardCode card)
            [ LoadDecklist player
                $ ArkhamDBDecklist mempty (InvestigatorId $ cdCardCode card) (toTitle card) Nothing
            ]
          | card <- availablePrologueInvestigators
          ]
      pure c
    CampaignStep (PrologueStepPart 2) -> do
      taken <- selectListMap unInvestigatorId Anyone
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
      players <- allPlayers
      pushAll
        $ crossOutRecordSetEntries MissingPersons prologueInvestigatorsNotTaken
        : map (story players) readings
      pure c
    CampaignStep (InterludeStep 2 mInterludeKey) -> do
      anySilverTwilight <- selectAny $ InvestigatorWithTrait SilverTwilight
      iids <- allInvestigatorIds
      players <- allPlayers
      lead <- getLeadPlayer
      let
        showThePriceOfProgress4 = mInterludeKey == Just ThePriceOfProgress4
        showThePriceOfProgress5 = mInterludeKey == Just ThePriceOfProgress5
        showThePriceOfProgress6 = mInterludeKey == Just ThePriceOfProgress6
        gainXp = map (\i -> GainXP i (toSource attrs) 2) iids
        lodgeChoices =
          [ Label "\"I refuse to be part of this\"" [CampaignStep (InterludeStepPart 2 mInterludeKey 7)]
          , Label "\"I agree\"" [CampaignStep (InterludeStepPart 2 mInterludeKey 8)]
          , Label "\"I agree\" (You are lying)" [CampaignStep (InterludeStepPart 2 mInterludeKey 9)]
          ]
      pushAll
        $ [ story players (if anySilverTwilight then thePriceOfProgress1 else thePriceOfProgress2)
          , story players thePriceOfProgress3
          ]
        <> ( guard showThePriceOfProgress4
              *> [ story players thePriceOfProgress4
                 , Record JosefDisappearedIntoTheMist
                 , Record TheInvestigatorsAreEnemiesOfTheLodge
                 , NextCampaignStep Nothing
                 ]
           )
        <> ( guard showThePriceOfProgress5
              *> [ Record TheInvestigatorsRescuedJosef
                 , storyWithChooseOne lead players thePriceOfProgress5 lodgeChoices
                 ]
              <> gainXp
           )
        <> ( guard showThePriceOfProgress6
              *> [Record JosefIsAliveAndWell, storyWithChooseOne lead players thePriceOfProgress6 lodgeChoices]
           )
      pure c
    CampaignStep (InterludeStepPart 2 _ 7) -> do
      pushAll [Record TheInvestigatorsAreEnemiesOfTheLodge, NextCampaignStep Nothing]
      pure c
    CampaignStep (InterludeStepPart 2 _ 8) -> do
      pushAll
        [Record TheInvestigatorsAreMembersOfTheLodge, AddChaosToken Cultist, NextCampaignStep Nothing]
      pure c
    CampaignStep (InterludeStepPart 2 _ 9) -> do
      pushAll
        [ Record TheInvestigatorsAreMembersOfTheLodge
        , AddChaosToken Cultist
        , Record TheInvestigatorsAreDeceivingTheLodge
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (InterludeStep 3 mInterludeKey) -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ story players theInnerCircle1
        , chooseOne
            lead
            [ Label "" [CampaignStep (InterludeStepPart 3 mInterludeKey 2)]
            , Label "" [CampaignStep (InterludeStepPart 3 mInterludeKey 3)]
            ]
        ]
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 2) -> do
      players <- allPlayers
      rescuedJosef <- getHasRecord TheInvestigatorsRescuedJosef
      toldLodgeAboutCoven <- getHasRecord TheInvestigatorsToldTheLodgeAboutTheCoven
      someMementos <- getRecordSet MementosDiscovered
      let mementos = mapMaybe (unrecorded @Memento) someMementos
      pushAll
        [ story players theInnerCircle2
        , crossOutRecordSetEntries MementosDiscovered (toList mementos)
        , CampaignStep
            (InterludeStepPart 3 mInterludeKey $ if rescuedJosef && toldLodgeAboutCoven then 4 else 5)
        ]
      pure c
    CampaignStep (InterludeStepPart 3 _ 3) -> do
      players <- allPlayers
      pushAll
        [ story players theInnerCircle3
        , Record TheInvestigatorsKeptsTheirMementosHidden
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 4) -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ story players theInnerCircle4
        , chooseUpToN
            lead
            3
            "Done asking question"
            [ Label "What is the creature?" [story players whatIsTheCreature]
            , Label "What do you want with the creature?" [story players whatDoYouWantWithTheCreature]
            , Label
                "What do the witches want with the creature?"
                [story players whatDoTheWitchesWantWithTheCreature]
            , Label
                "Did you know about the creature before the charity gala?"
                [story players didYouKnowAboutTheCreatureBeforeTheCharityGala]
            , Label
                "Where are the four missing people from the charity gala?"
                [story players whereAreTheFourMissingPeopleFromTheCharityGala]
            ]
        , CampaignStep (InterludeStepPart 3 mInterludeKey 6)
        ]
      pure c
    CampaignStep (InterludeStepPart 3 _ 5) -> do
      players <- allPlayers
      pushAll [story players theInnerCircle5, NextCampaignStep Nothing]
      pure c
    CampaignStep (InterludeStepPart 3 _ 6) -> do
      players <- allPlayers
      pushAll
        [ story players theInnerCircle6
        , Record TheInvestigatorsWereInductedIntoTheInnerCircle
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (InterludeStep 4 _) -> do
      acceptedYourFate <- getHasRecord YouHaveAcceptedYourFate
      askedAnetteForAssistance <- getHasRecord TheInvestigatorsAskedAnetteForAssistance
      askedSanfordForAssistance <- getHasRecord TheInvestigatorsAskedSanfordForAssistance
      mementosDiscovered <- getMementosDiscoveredCount
      doomDrawsEverCloser <- getHasRecord DoomDrawsEverCloser
      hasBlackBook <- isJust <$> getOwner Assets.theBlackBook
      players <- allPlayers
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
      pushAll
        $ [ story players twistOfFate1
          , RecordCount ThePathWindsBeforeYou total
          ]
        <> [ AddChaosToken $ fromDifficulty MinusThree MinusFour MinusFive MinusSix (campaignDifficulty attrs)
           | askedAnetteForAssistance || askedSanfordForAssistance
           ]
        <> [ story players twistOfFate2
           , NextCampaignStep Nothing
           ]
      pure c
    CampaignStep EpilogueStep -> do
      arrestedAnette <- getHasRecord TheInvestigatorsArrestedAnette
      assumedControlOfTheLodge <- getHasRecord TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
      survivedTheWatchersEmbrace <- getHasRecord TheInvestigatorsSurvivedTheWatchersEmbrace
      signedTheBlackBook <- getHasRecord TheInvestigatorsSignedTheBlackBookOfAzathoth
      players <- allPlayers
      pushAll
        $ [story players epilogueArrestedAnette | arrestedAnette]
        <> [story players epilogueAssumedControlOfTheLodge | assumedControlOfTheLodge]
        <> [story players epilogueSurvivedTheWatchersEmbrace | survivedTheWatchersEmbrace]
        <> [story players epilogueSignedTheBlackBook | signedTheBlackBook]
        <> [GameOver]

      pure c
    HandleOption option -> do
      lead <- getLeadPlayer
      investigators <- allInvestigators
      case option of
        TakeBlackBook -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.theBlackBook
        TakePuzzleBox -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.puzzleBox
        ProceedToInterlude3 -> pure ()
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> defaultCampaignRunner msg c
