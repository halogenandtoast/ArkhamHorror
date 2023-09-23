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
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers
import Arkham.Helpers.Campaign
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (Trait (SilverTwilight))

newtype Metadata = Metadata
  { prologueInvestigators :: Map InvestigatorId InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheCircleUndone = TheCircleUndone (CampaignAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

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
    (TheCircleUndone . (`with` Metadata mempty))
    (CampaignId "05")
    "The Circle Undone"
    difficulty
    (chaosBagContents difficulty)
    $ logL
    .~ mkCampaignLog
      { campaignLogRecordedSets =
          singletonMap MissingPersons
            $ map (recorded . unInvestigatorId) allPrologueInvestigators
      }

allPrologueInvestigators :: [InvestigatorId]
allPrologueInvestigators = ["05046", "05047", "05048", "05049"]

instance RunMessage TheCircleUndone where
  runMessage msg c@(TheCircleUndone (attrs `With` metadata)) = case msg of
    CampaignStep PrologueStep -> do
      investigatorIds <- allInvestigatorIds
      pushAll
        $ story investigatorIds prologue
        : [ CampaignStep (InvestigatorCampaignStep iid PrologueStep)
          | iid <- investigatorIds
          ]
          <> [ story investigatorIds intro
             , CampaignStep (PrologueStepPart 2)
             , NextCampaignStep Nothing
             ]
      pure c
    CampaignStep (InvestigatorCampaignStep iid PrologueStep) -> do
      let
        availablePrologueInvestigators =
          filter
            (`notElem` toList (prologueInvestigators metadata))
            allPrologueInvestigators
      push
        $ questionLabel
          "Choose one of the following neutral investigators to control for the duration of this prologue"
          iid
        $ ChooseOne
          [ CardLabel
            (unInvestigatorId pId)
            [BecomePrologueInvestigator iid pId]
          | pId <- availablePrologueInvestigators
          ]
      pure c
    BecomePrologueInvestigator iid pId -> do
      pure
        . TheCircleUndone
          $ attrs
          `With` metadata
            { prologueInvestigators =
                insertMap
                  iid
                  pId
                  (prologueInvestigators metadata)
            }
    CampaignStep (PrologueStepPart 2) -> do
      let
        prologueInvestigatorsNotTaken =
          map unInvestigatorId
            $ allPrologueInvestigators
            \\ toList
              (prologueInvestigators metadata)
        readingFor = \case
          "05046" -> gavriellaIntro
          "05047" -> jeromeIntro
          "05048" -> valentinoIntro
          "05049" -> pennyIntro
          _ -> error "Invalid prologue investigator"
        readings = map readingFor $ toList (prologueInvestigators metadata)
      investigatorIds <- getInvestigatorIds
      pushAll
        $ crossOutRecordSetEntries MissingPersons prologueInvestigatorsNotTaken
        : map (story investigatorIds) readings
      pure c
    CampaignStep (InterludeStep 2 mInterludeKey) -> do
      anySilverTwilight <- selectAny $ InvestigatorWithTrait SilverTwilight
      iids <- allInvestigatorIds
      lead <- getLead
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
        $ [ story iids (if anySilverTwilight then thePriceOfProgress1 else thePriceOfProgress2)
          , story iids thePriceOfProgress3
          ]
        <> ( guard showThePriceOfProgress4
              *> [ story iids thePriceOfProgress4
                 , Record JosefDisappearedIntoTheMist
                 , Record TheInvestigatorsAreEnemiesOfTheLodge
                 , NextCampaignStep Nothing
                 ]
           )
        <> ( guard showThePriceOfProgress5
              *> [ Record TheInvestigatorsRescuedJosef
                 , storyWithChooseOne lead iids thePriceOfProgress5 lodgeChoices
                 ]
                <> gainXp
           )
        <> ( guard showThePriceOfProgress6
              *> [Record JosefIsAliveAndWell, storyWithChooseOne lead iids thePriceOfProgress6 lodgeChoices]
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
      iids <- allInvestigatorIds
      lead <- getLead
      pushAll
        [ story iids theInnerCircle1
        , chooseOne
            lead
            [ Label "" [CampaignStep (InterludeStepPart 3 mInterludeKey 2)]
            , Label "" [CampaignStep (InterludeStepPart 3 mInterludeKey 3)]
            ]
        ]
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 2) -> do
      iids <- allInvestigatorIds
      rescuedJosef <- getHasRecord TheInvestigatorsRescuedJosef
      toldLodgeAboutCoven <- getHasRecord TheInvestigatorsToldTheLodgeAboutTheCoven
      someMementos <- getRecordSet MementosDiscovered
      let mementos = mapMaybe (unrecorded @Memento) someMementos
      pushAll
        [ story iids theInnerCircle2
        , crossOutRecordSetEntries MementosDiscovered (toList mementos)
        , CampaignStep
            (InterludeStepPart 3 mInterludeKey $ if rescuedJosef && toldLodgeAboutCoven then 4 else 5)
        ]
      pure c
    CampaignStep (InterludeStepPart 3 _ 3) -> do
      iids <- allInvestigatorIds
      pushAll
        [ story iids theInnerCircle3
        , Record TheInvestigatorsKeptsTheirMementosHidden
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (InterludeStepPart 3 mInterludeKey 4) -> do
      iids <- allInvestigatorIds
      lead <- getLead
      pushAll
        [ story iids theInnerCircle4
        , chooseUpToN
            lead
            3
            "Done asking question"
            [ Label "What is the creature?" [story iids whatIsTheCreature]
            , Label "What do you want with the creature?" [story iids whatDoYouWantWithTheCreature]
            , Label "What do the witches want with the creature?" [story iids whatDoTheWitchesWantWithTheCreature]
            , Label
                "Did you know about the creature before the charity gala?"
                [story iids didYouKnowAboutTheCreatureBeforeTheCharityGala]
            , Label
                "Where are the four missing people from the charity gala?"
                [story iids whereAreTheFourMissingPeopleFromTheCharityGala]
            ]
        , CampaignStep (InterludeStepPart 3 mInterludeKey 6)
        ]
      pure c
    CampaignStep (InterludeStepPart 3 _ 5) -> do
      iids <- allInvestigatorIds
      pushAll [story iids theInnerCircle5, NextCampaignStep Nothing]
      pure c
    CampaignStep (InterludeStepPart 3 _ 6) -> do
      iids <- allInvestigatorIds
      pushAll
        [ story iids theInnerCircle6
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
      investigators <- allInvestigators
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
        $ [ story investigators twistOfFate1
          , RecordCount ThePathWindsBeforeYou total
          ]
        <> [ AddChaosToken $ fromDifficulty MinusThree MinusFour MinusFive MinusSix (campaignDifficulty attrs)
           | askedAnetteForAssistance || askedSanfordForAssistance
           ]
        <> [ story investigators twistOfFate2
           , NextCampaignStep Nothing
           ]
      pure c
    CampaignStep EpilogueStep -> do
      arrestedAnette <- getHasRecord TheInvestigatorsArrestedAnette
      assumedControlOfTheLodge <- getHasRecord TheInvestigatorsAssumedControlOfTheSilverTwilightLodge
      survivedTheWatchersEmbrace <- getHasRecord TheInvestigatorsSurvivedTheWatchersEmbrace
      signedTheBlackBook <- getHasRecord TheInvestigatorsSignedTheBlackBookOfAzathoth
      investigators <- allInvestigators
      pushAll
        $ [story investigators epilogueArrestedAnette | arrestedAnette]
        <> [story investigators epilogueAssumedControlOfTheLodge | assumedControlOfTheLodge]
        <> [story investigators epilogueSurvivedTheWatchersEmbrace | survivedTheWatchersEmbrace]
        <> [story investigators epilogueSignedTheBlackBook | signedTheBlackBook]
        <> [GameOver]

      pure c
    PreScenarioSetup -> do
      case mapToList (prologueInvestigators metadata) of
        [] -> pure ()
        xs -> pushAll $ map (uncurry BecomePrologueInvestigator) xs
      pure c
    EndOfScenario _ -> do
      pure
        . TheCircleUndone
          $ attrs
          `With` metadata
            { prologueInvestigators = mempty
            }
    ResetGame -> do
      case mapToList (prologueInvestigators metadata) of
        [] -> defaultCampaignRunner msg c
        xs -> do
          for_ xs $ \(iid, _) -> push $ LoadDeck iid $ Deck []
          pure c
    HandleOption option -> do
      lead <- getLead
      investigators <- allInvestigators
      case option of
        TakeBlackBook -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.theBlackBook
        TakePuzzleBox -> push $ forceAddCampaignCardToDeckChoice lead investigators Assets.puzzleBox
        ProceedToInterlude3 -> pure ()
        _ -> error $ "Unhandled option: " <> show option
      pure c
    _ -> defaultCampaignRunner msg c
