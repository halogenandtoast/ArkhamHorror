module Arkham.Scenario.Scenarios.TheSearchForKadath (TheSearchForKadath (..), theSearchForKadath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheSearchForKadath.FlavorText
import Arkham.Scenarios.TheSearchForKadath.Helpers
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
    [ ".             baharna        ulthar    .           serannian"
    , "namelessRuins mtNgranek      skaiRiver dylathLeen  celephaïs"
    , ".             .              sarnath   kadatheron  hazuthKleg"
    , ".             .              ruinsOfIb .           templeOfUnattainableDesires"
    , "zulanThek     forbiddenLands ilekVad   .           ."
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

readInvestigatorDefeat :: HasGame m => m [Message]
readInvestigatorDefeat = do
  defeatedInvestigatorIds <- select DefeatedInvestigator
  if null defeatedInvestigatorIds
    then pure []
    else do
      resigned <- select ResignedInvestigator
      defeatedPlayers <- traverse getPlayer defeatedInvestigatorIds
      mRandolphCarterOwner <- getOwner Assets.randolphCarterExpertDreamer
      lead <- getLeadPlayer

      let
        randolphMessages = case mRandolphCarterOwner of
          Just owner
            | owner `elem` defeatedInvestigatorIds
            , notNull resigned ->
                [ RemoveCampaignCard Assets.randolphCarterExpertDreamer
                , forceAddCampaignCardToDeckChoice lead resigned Assets.randolphCarterExpertDreamer
                ]
          _ -> []
      pure
        $ [story defeatedPlayers investigatorDefeat]
        <> [RecordForInvestigator iid WasCaptured | iid <- defeatedInvestigatorIds]
        <> randolphMessages

instance RunMessage TheSearchForKadath where
  runMessage msg s@(TheSearchForKadath attrs) = case msg of
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ do
        push $ SetChaosTokens standaloneChaosTokens
      pure s
    StandaloneSetup -> do
      pure
        . TheSearchForKadath
        $ attrs
        & standaloneCampaignLogL
        <>~ standaloneCampaignLog
    PreScenarioSetup -> do
      players <- allPlayers
      blackCatAtYourSide <- getHasRecord TheBlackCatIsAtYourSide
      parleyed <- getHasRecord TheInvestigatorsParleyedWithTheZoogs
      saved <- getHasRecord TheInvestigatorsWereSavedByRandolphCarder
      withLuke <- selectAny $ InvestigatorWithTitle "Luke Robinson"
      pushAll $ story players intro1 -- 1
        : (guard blackCatAtYourSide *> [story players intro2]) -- 2
          <> (guard withLuke *> [story players intro3]) -- 3
          <> (guard (not withLuke) *> [story players intro4]) -- 4
          <> [DoStep (if parleyed || saved then 5 else 6) PreScenarioSetup] -- 5 || 6
      pure s
    DoStep 5 PreScenarioSetup -> do
      lead <- getLeadPlayer
      players <- allPlayers
      push
        $ storyWithChooseOne
          lead
          players
          intro5
          [ Label "Leave empty-handed" [story players intro7]
          , Label
              "Force your way into the temple."
              [story players intro8, Record TheInvestigatorsForcedTheirWayIntoTheTemple]
          ]
      pure s
    DoStep 6 PreScenarioSetup -> do
      players <- allPlayers
      investigators <- allInvestigators
      parleyed <- getHasRecord TheInvestigatorsParleyedWithTheZoogs
      pushAll
        $ [story players intro6, story players intro9, IncrementRecordCount EvidenceOfKadath 1]
        <> ( guard parleyed
              *> ( [story players intro10, IncrementRecordCount EvidenceOfKadath 1]
                    <> map (\iid -> GainXP iid (toSource attrs) 2) investigators
                 )
           )
        <> (guard (not parleyed) *> [story players intro11])
      pure s
    Setup -> do
      let
        setAsideCards =
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
      let excludes = setAsideCards <> [Locations.dylathLeen, Locations.skaiRiver, Locations.ulthar]

      encounterDeck <-
        buildEncounterDeckExcluding
          excludes
          [ EncounterSet.TheSearchForKadath
          , EncounterSet.AgentsOfNyarlathotep
          , EncounterSet.Corsairs
          , EncounterSet.Dreamlands
          , EncounterSet.WhispersOfHypnos
          , EncounterSet.Zoogs
          ]

      setAside <- genCards setAsideCards

      (ulthar, placeUlthar) <- placeLocationCard Locations.ulthar
      otherPlacements <- placeLocationCards_ [Locations.skaiRiver, Locations.dylathLeen]

      agendas <- genCards [Agendas.journeyAcrossTheDreamlands, Agendas.agentsOfTheOuterGods]
      acts <-
        genCards
          [ Acts.kingdomOfTheSkai
          , Acts.theIsleOfOriab
          , Acts.theDoomThatCameBefore
          , Acts.seekOutTheNight
          , Acts.theKingsDecree
          ]
      lead <- getLead
      virgil <- genCard Assets.virgilGray

      pushAll
        $ [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck, placeUlthar]
        <> otherPlacements
        <> [MoveAllTo (toSource attrs) ulthar]
        <> [TakeControlOfSetAsideAsset lead virgil]

      TheSearchForKadath
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAside)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> void $ runMaybeT $ do
          Action.Investigate <- MaybeT getSkillTestAction
          LocationTarget lid <- MaybeT getSkillTestTarget
          lift $ push $ roundModifier Cultist lid (ShroudModifier $ if isEasyStandard attrs then 1 else 2)
        Tablet -> do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label "Take 1 damage and 1 horror" [assignDamageAndHorror iid Tablet 1 1]
              , Label "Place 1 doom on the current agenda" [placeDoomOnAgenda]
              ]
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        ElderThing -> void $ runMaybeT $ do
          Action.Investigate <- MaybeT getSkillTestAction
          lift $ push $ skillTestModifier ElderThing iid (DiscoveredClues 1)
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
      let meta = toResult (scenarioMeta attrs)
      let meta' = meta {regions = regions meta <> [region]}

      leadId <- getLead
      investigators <- getInvestigators
      victoryLocations <- select $ LocationWithVictory <> LocationWithoutClues
      locations <- filter (`notElem` victoryLocations) <$> select Anywhere

      tenebrousNightgaunts <- select $ enemyIs Enemies.tenebrousNightgaunt

      player <- getPlayer leadId
      let
        nightgauntMessages =
          guard (notNull tenebrousNightgaunts)
            *> [ chooseOneAtATime
                  player
                  [AbilityLabel leadId (mkAbility t 1 $ forced NotAnyWindow) [] [] | t <- tenebrousNightgaunts]
               ]

      case region of
        Oriab -> do
          (baharna, placeBaharna) <- placeSetAsideLocation Locations.baharna
          placeOriabRest <- placeSetAsideLocations [Locations.mtNgranek, Locations.namelessRuins]

          pushAll
            $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
            <> nightgauntMessages
            <> [placeBaharna, MoveAllTo (toSource attrs) baharna]
            <> placeOriabRest
            <> map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
            <> [ search
                  leadId
                  (toSource attrs)
                  EncounterDeckTarget
                  [fromDeck]
                  (cardIs Enemies.nightriders)
                  (DeferSearchedToTarget $ toTarget attrs)
               , AdvanceToAct 1 Acts.theIsleOfOriab A (toSource attrs)
               , DoStep 1 msg
               ]
        Mnar -> do
          (kadatheron, placeKadatheron) <- placeSetAsideLocation Locations.kadatheron
          (ruinsOfIb, placeRuinsOfIb) <- placeSetAsideLocation Locations.ruinsOfIb
          placeSarnath <- placeSetAsideLocation_ Locations.sarnath
          beingsOfIb <- getSetAsideCard Enemies.beingsOfIb
          createBeingsOfIb <- createEnemyAt_ beingsOfIb ruinsOfIb Nothing

          pushAll
            $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
            <> nightgauntMessages
            <> [placeKadatheron, MoveAllTo (toSource attrs) kadatheron]
            <> [placeRuinsOfIb, placeSarnath, createBeingsOfIb]
            <> map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
            <> [AdvanceToAct 1 Acts.theDoomThatCameBefore A (toSource attrs), DoStep 1 msg]
        ForbiddenLands -> do
          (ilekVad, placeIlekVad) <- placeSetAsideLocation Locations.ilekVad
          (forbiddenLands, placeForbiddenLands) <- placeSetAsideLocation Locations.forbiddenLands
          (zulanThek, placeZulanThek) <- placeSetAsideLocation Locations.zulanThek
          stalkingManticore <- getSetAsideCard Enemies.stalkingManticore
          hordeOfNight <- getSetAsideCard Enemies.hordeOfNight
          createStalkingManticore <- createEnemyAt_ stalkingManticore forbiddenLands Nothing
          createHordeOfNight <- createEnemyAt_ hordeOfNight zulanThek Nothing

          pushAll
            $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
            <> nightgauntMessages
            <> [placeIlekVad, MoveAllTo (toSource attrs) ilekVad]
            <> [placeForbiddenLands, placeZulanThek, createStalkingManticore, createHordeOfNight]
            <> map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
            <> [AdvanceToAct 1 Acts.seekOutTheNight A (toSource attrs), DoStep 1 msg]
        TimelessRealm -> do
          (celephais, placeCelephais) <- placeSetAsideLocation Locations.celephais
          placeTimlessRealmRest <- placeSetAsideLocations [Locations.serannian, Locations.hazuthKleg]
          theCrawlingMist <- getSetAsideCard Enemies.theCrawlingMist

          pushAll
            $ map (InvestigatorDiscardAllClues ScenarioSource) investigators
            <> nightgauntMessages
            <> [placeCelephais, MoveAllTo (toSource attrs) celephais]
            <> placeTimlessRealmRest
            <> map (AddToVictory . toTarget) victoryLocations
            <> map RemoveLocation locations
            <> [ ShuffleCardsIntoDeck Deck.EncounterDeck [theCrawlingMist]
               , search
                  leadId
                  attrs
                  EncounterDeckTarget
                  [fromDeck]
                  (cardIs Enemies.priestOfAThousandMasks)
                  (DeferSearchedToTarget $ toTarget attrs)
               , AdvanceToAct 1 Acts.theKingsDecree A (toSource attrs)
               , DoStep 1 msg
               ]
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
          msgs <- readInvestigatorDefeat
          players <- allPlayers
          gainXp <- toGainXp attrs getXp
          evidence <- getSignsOfTheGods
          pushAll
            $ msgs
            <> [story players resolutionText]
            <> gainXp
            <> [ IncrementRecordCount EvidenceOfKadath evidence
               , Record VirgilWasCaptured
               , Record randolphStatus
               , EndOfGame Nothing
               ]
      pure s
    _ -> TheSearchForKadath <$> runMessage msg attrs
