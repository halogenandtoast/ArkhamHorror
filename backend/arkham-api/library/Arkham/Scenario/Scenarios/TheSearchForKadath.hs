module Arkham.Scenario.Scenarios.TheSearchForKadath (TheSearchForKadath (..), theSearchForKadath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCardCode))
import Arkham.Exception
import Arkham.Helpers.Modifiers hiding (roundModifier, skillTestModifier)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (
  defeated,
  forceAddCampaignCardToDeckChoice,
  roundModifier,
  skillTestModifier,
 )
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (metaL)
import Arkham.Scenarios.TheSearchForKadath.FlavorText
import Arkham.Scenarios.TheSearchForKadath.Helpers
import Arkham.Strategy
import Arkham.Trait (Trait (City))

newtype TheSearchForKadath = TheSearchForKadath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForKadath :: Difficulty -> TheSearchForKadath
theSearchForKadath difficulty =
  scenarioWith
    TheSearchForKadath
    "06119"
    "The Search for Kadath"
    difficulty
    [ ".             baharna        ulthar    .           serannian  ."
    , "namelessRuins mtNgranek      skaiRiver dylathLeen  celephaïs  cityWhichAppearsOnNoMap"
    , ".             .              sarnath   kadatheron  hazuthKleg templeOfUnattainableDesires"
    , ".             .              ruinsOfIb .           .          ."
    , "zulanThek     forbiddenLands ilekVad   .           .          ."
    ]
    (metaL .~ toJSON (Meta []))

instance HasChaosTokenValue TheSearchForKadath where
  getChaosTokenValue iid tokenFace (TheSearchForKadath attrs) = case tokenFace of
    Skull -> do
      n <- getSignsOfTheGods
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ ChaosTokenValue ElderThing (PositiveModifier $ if isEasyStandard attrs then 2 else 1)
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , Cultist
  , Tablet
  , Tablet
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog {campaignLogRecorded = setFromList [TheInvestigatorsWereSavedByRandolphCarder]}

readInvestigatorDefeat :: ReverseQueue m => m ()
readInvestigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    resigned <- select ResignedInvestigator
    storyOnly defeated investigatorDefeat
    for_ defeated $ \iid -> push $ RecordForInvestigator iid WasCaptured
    withOwner Assets.randolphCarterExpertDreamer $ \owner -> do
      when ((owner `elem` defeated) && notNull resigned) do
        removeCampaignCard Assets.randolphCarterExpertDreamer
        forceAddCampaignCardToDeckChoice resigned Assets.randolphCarterExpertDreamer

instance RunMessage TheSearchForKadath where
  runMessage msg s@(TheSearchForKadath attrs) = runQueueT $ case msg of
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure . TheSearchForKadath $ attrs & standaloneCampaignLogL <>~ standaloneCampaignLog
    PreScenarioSetup -> do
      story intro1
      blackCatAtYourSide <- getHasRecord TheBlackCatIsAtYourSide
      if blackCatAtYourSide
        then doStep 2 PreScenarioSetup
        else do
          withLuke <- selectAny $ InvestigatorWithTitle "Luke Robinson"
          doStep (if withLuke then 3 else 4) PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      story intro2
      withLuke <- selectAny $ InvestigatorWithTitle "Luke Robinson"
      doStep (if withLuke then 3 else 4) PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> do
      story intro3
      parleyed <- getHasRecord TheInvestigatorsParleyedWithTheZoogs
      doStep (if parleyed then 5 else 6) PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> do
      story intro4
      parleyed <- getHasRecord TheInvestigatorsParleyedWithTheZoogs
      doStep (if parleyed then 5 else 6) PreScenarioSetup
      pure s
    DoStep 5 PreScenarioSetup -> do
      storyWithChooseOneM intro5 do
        labeled "Leave empty-handed" $ doStep 7 PreScenarioSetup
        labeled "Force your way into the temple." $ doStep 8 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> do
      story intro6
      doStep 9 PreScenarioSetup
      pure s
    DoStep 7 PreScenarioSetup -> do
      story intro7
      pure s
    DoStep 8 PreScenarioSetup -> do
      story intro8
      record TheInvestigatorsForcedTheirWayIntoTheTemple
      doStep 9 PreScenarioSetup
      pure s
    DoStep 9 PreScenarioSetup -> do
      parleyed <- getHasRecord TheInvestigatorsParleyedWithTheZoogs
      story intro9
      push $ IncrementRecordCount EvidenceOfKadath 1
      doStep (if parleyed then 10 else 11) PreScenarioSetup
      pure s
    DoStep 10 PreScenarioSetup -> do
      story intro10
      push $ IncrementRecordCount EvidenceOfKadath 1
      eachInvestigator \iid -> push $ GainXP iid (toSource attrs) 2
      pure s
    DoStep 11 PreScenarioSetup -> do
      story intro11
      pure s
    Setup -> runScenarioSetup TheSearchForKadath attrs do
      gather Set.TheSearchForKadath
      gather Set.AgentsOfNyarlathotep
      gather Set.Corsairs
      gather Set.Dreamlands
      gather Set.WhispersOfHypnos
      gather Set.Zoogs

      setAside
        [ Enemies.catsOfUlthar
        , Enemies.stalkingManticore
        , Enemies.theCrawlingMist
        , Enemies.hordeOfNight
        , Enemies.beingsOfIb
        , Enemies.tenebrousNightgaunt
        , Enemies.tenebrousNightgaunt
        , Enemies.corsairOfLeng
        , Enemies.corsairOfLeng
        , Enemies.priestOfAThousandMasks
        , Enemies.priestOfAThousandMasks
        , Enemies.priestOfAThousandMasks
        , Locations.baharna
        , Locations.namelessRuins
        , Locations.mtNgranek
        , Locations.sarnath
        , Locations.kadatheron
        , Locations.ruinsOfIb
        , Locations.zulanThek
        , Locations.forbiddenLands
        , Locations.ilekVad
        , Locations.serannian
        , Locations.celephais
        , Locations.hazuthKleg
        , Locations.cityWhichAppearsOnNoMap
        , Locations.templeOfUnattainableDesires
        ]

      startAt =<< place Locations.ulthar

      placeAll [Locations.skaiRiver, Locations.dylathLeen]

      setAgendaDeck [Agendas.journeyAcrossTheDreamlands, Agendas.agentsOfTheOuterGods]
      setActDeck
        [ Acts.kingdomOfTheSkai
        , Acts.theIsleOfOriab
        , Acts.theDoomThatCameBefore
        , Acts.seekOutTheNight
        , Acts.theKingsDecree
        ]
      lead <- getLead
      beginWithStoryAsset lead Assets.virgilGray
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> void $ runMaybeT $ do
          Action.Investigate <- MaybeT getSkillTestAction
          LocationTarget lid <- MaybeT getSkillTestTarget
          lift $ roundModifier Cultist lid (ShroudModifier $ if isEasyStandard attrs then 1 else 2)
        Tablet -> do
          chooseOneM iid do
            labeled "Take 1 damage and 1 horror" $ assignDamageAndHorror iid Tablet 1 1
            labeled "Place 1 doom on the current agenda" $ placeDoomOnAgenda 1
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      withSkillTest \sid ->
        case token.face of
          ElderThing -> void $ runMaybeT $ do
            Action.Investigate <- MaybeT getSkillTestAction
            lift $ skillTestModifier sid ElderThing iid (DiscoveredClues 1)
          _ -> pure ()
      pure s
    DoStep 1 (SetScenarioMeta _) -> do
      tenebrousNightgaunts <- select $ enemyIs Enemies.tenebrousNightgaunt <> EnemyWithPlacement Unplaced
      when (notNull tenebrousNightgaunts) $ do
        cities <- select $ LocationWithTrait City
        lead <- getLeadPlayer
        pushAll $ case cities of
          [c] -> [PlaceEnemy t $ AtLocation c | t <- tenebrousNightgaunts]
          _ ->
            [ Ask lead
              $ QuestionLabel "Place Tenebrous Nightgaunt in city location" Nothing
              $ ChooseOne [targetLabel c [PlaceEnemy t $ AtLocation c] | c <- cities]
            | t <- tenebrousNightgaunts
            ]

      pure s
    SetScenarioMeta value -> do
      let region = toResult value
      let meta = toResult attrs.meta
      let meta' = meta {regions = regions meta <> [region]}

      leadId <- getLead
      investigators <- getInvestigators
      victoryLocations <- select $ LocationWithVictory <> LocationWithoutClues
      locations <- filter (`notElem` victoryLocations) <$> select Anywhere

      tenebrousNightgaunts <- selectWithField EnemyCardCode $ enemyIs Enemies.tenebrousNightgaunt

      let
        nightgauntMessages =
          when (notNull tenebrousNightgaunts) do
            chooseOneAtATime
              leadId
              [ AbilityLabel leadId (mkAbility (SourceableWithCardCode cc t) 1 $ forced NotAnyWindow) [] [] []
              | (t, cc) <- tenebrousNightgaunts
              ]

      case region of
        Oriab -> do
          baharna <- placeSetAsideLocation Locations.baharna
          placeSetAsideLocations_ [Locations.mtNgranek, Locations.namelessRuins]

          pushAll $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
          nightgauntMessages
          selectEach (not_ $ enemyIs Enemies.tenebrousNightgaunt) $ toDiscard ScenarioSource
          moveAllTo (toSource attrs) baharna
          pushAll
            $ map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
          search leadId attrs EncounterDeckTarget [fromDeck] (basicCardIs Enemies.nightriders)
            $ defer attrs IsNotDraw
          push $ AdvanceToAct 1 Acts.theIsleOfOriab A (toSource attrs)
          doStep 1 msg
        Mnar -> do
          kadatheron <- placeSetAsideLocation Locations.kadatheron
          ruinsOfIb <- placeSetAsideLocation Locations.ruinsOfIb
          placeSetAsideLocation_ Locations.sarnath

          pushAll $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
          nightgauntMessages
          selectEach (not_ $ enemyIs Enemies.tenebrousNightgaunt) $ toDiscard ScenarioSource
          beingsOfIb <- getSetAsideCard Enemies.beingsOfIb
          createEnemyAt_ beingsOfIb ruinsOfIb
          moveAllTo (toSource attrs) kadatheron
          pushAll
            $ map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
            <> [AdvanceToAct 1 Acts.theDoomThatCameBefore A (toSource attrs), DoStep 1 msg]
        ForbiddenLands -> do
          ilekVad <- placeSetAsideLocation Locations.ilekVad
          forbiddenLands <- placeSetAsideLocation Locations.forbiddenLands
          zulanThek <- placeSetAsideLocation Locations.zulanThek

          pushAll $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
          nightgauntMessages
          selectEach (not_ $ enemyIs Enemies.tenebrousNightgaunt) $ toDiscard ScenarioSource
          stalkingManticore <- getSetAsideCard Enemies.stalkingManticore
          hordeOfNight <- getSetAsideCard Enemies.hordeOfNight
          createEnemyAt_ stalkingManticore forbiddenLands
          createEnemyAt_ hordeOfNight zulanThek
          moveAllTo (toSource attrs) ilekVad
          pushAll
            $ map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
            <> [AdvanceToAct 1 Acts.seekOutTheNight A (toSource attrs), DoStep 1 msg]
        TimelessRealm -> do
          celephais <- placeSetAsideLocation Locations.celephais
          placeSetAsideLocations_ [Locations.serannian, Locations.hazuthKleg]

          pushAll $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
          nightgauntMessages
          selectEach (not_ $ enemyIs Enemies.tenebrousNightgaunt) $ toDiscard ScenarioSource
          theCrawlingMist <- getSetAsideCard Enemies.theCrawlingMist
          moveAllTo (toSource attrs) celephais
          pushAll
            $ map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations

          shuffleCardsIntoDeck Deck.EncounterDeck [theCrawlingMist]
          search leadId attrs EncounterDeckTarget [fromDeck] (basicCardIs Enemies.priestOfAThousandMasks)
            $ defer attrs IsNotDraw
          push $ AdvanceToAct 1 Acts.theKingsDecree A (toSource attrs)
          doStep 1 msg
      pure $ TheSearchForKadath $ attrs & metaL .~ toJSON meta'
    ScenarioResolution r -> do
      case r of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          push $ if anyResigned then R1 else R2
        Resolution n -> do
          let
            (resolutionText, randolphStatus) = case n of
              1 -> (resolution1, RandolphEludedCapture)
              2 -> (resolution2, RandolphWasCaptured)
              other -> throw $ UnknownResolution $ Resolution other
          readInvestigatorDefeat
          evidence <- getSignsOfTheGods
          story resolutionText
          allGainXp attrs
          push $ IncrementRecordCount EvidenceOfKadath evidence
          record VirgilWasCaptured
          record randolphStatus
          endOfScenario
      pure s
    _ -> TheSearchForKadath <$> liftRunMessage msg attrs
