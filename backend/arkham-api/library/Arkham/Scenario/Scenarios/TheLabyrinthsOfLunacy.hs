module Arkham.Scenario.Scenarios.TheLabyrinthsOfLunacy (theLabyrinthsOfLunacy) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (ScenarioAttrs (..))
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Meta

newtype TheLabyrinthsOfLunacy = TheLabyrinthsOfLunacy ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- FOURMOLU_DISABLE -}
theLabyrinthsOfLunacy :: Difficulty -> TheLabyrinthsOfLunacy
theLabyrinthsOfLunacy difficulty =
  sideStory
    TheLabyrinthsOfLunacy
    "70001"
    "The Labyrinths of Lunacy"
    difficulty
    [ ".                  chamberOfRain      chamberOfSecrets   chamberOfNight"
    , ".                  chamberOfSorrows   .                  chamberOfRegret"
    , "labyrinthineHalls1 labyrinthineHalls2 labyrinthineHalls3 ."
    , "chamberOfDecay     chamberOfRot       chamberOfHunger    ."
    , ".                  chamberOfPoison    .                  ."
    , ".                  abandonedWarehouse .                  ."
    ]

standardTokens, hardTokens :: [ChaosTokenFace]
standardTokens =
  [ PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , MinusThree , MinusFour , MinusFive , Skull , Skull , AutoFail , ElderSign
  ]
hardTokens =
  [ PlusOne , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusTwo
  , MinusThree , MinusFour , MinusFive , MinusSix , Skull , Skull , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

-- Each group adds 2 of its own chaos tokens to the bag during setup.
groupChaosToken :: Group -> ChaosTokenFace
groupChaosToken = \case
  GroupA -> ElderThing
  GroupB -> Tablet
  GroupC -> Cultist

instance HasChaosTokenValue TheLabyrinthsOfLunacy where
  getChaosTokenValue iid tokenFace (TheLabyrinthsOfLunacy attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 1
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheLabyrinthsOfLunacy where
  runMessage msg s@(TheLabyrinthsOfLunacy attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      let meta =
            if scenarioTimesPlayed attrs > 0
              then toResult attrs.meta
              else initialMeta GroupA
      let remaining = filter (`notElem` playedGroups meta) [minBound ..]
      storyWithChooseOneM' (h "title" >> p "chooseGroup") do
        for_ remaining \g -> do
          popScope $ labeled' (groupLabel g) do
            push $ SetScenarioMeta $ toJSON $ meta {currentGroup = g}
            scope "intro" $ flavor do
              h "title"
              p $ case g of
                GroupA -> "groupA"
                GroupB -> "groupB"
                GroupC -> "groupC"
              p "note"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ if isEasyStandard attrs then standardTokens else hardTokens
      pure s
    Setup -> runScenarioSetup TheLabyrinthsOfLunacy attrs do
      let meta = toResult attrs.meta
      let grp = currentGroup meta
      setup $ ul do
        li "gatherSets"
        li.validate False "epicMultiplayer"
        li.validate True "singleGroup"
        li.validate (grp == GroupA) "actDeckGroupA"
        li.validate (grp == GroupB) "actDeckGroupB"
        li.validate (grp == GroupC) "actDeckGroupC"
        li.validate (grp == GroupA) "chamberOfSecrets"
        li.validate (grp == GroupB) "chamberOfRainAndSorrows"
        li.validate (grp == GroupC) "chamberOfNightAndRegret"
        li "setAside"
        li.validate (grp == GroupA) "keyOfMysteries"
        li.validate (grp == GroupC) "hiddenChamberOfSecrets"
        li "eixodolonsNote"
        li "addTokens"
        unscoped $ li "shuffleRemainder"

      gather Set.TheLabyrinthsOfLunacy
      gather Set.LabyrinthsOfLunacySingleGroup

      setAgendaDeck
        [ Agendas.awakeningTheLabyrinthsOfLunacy
        , Agendas.agonyAndDespair
        , Agendas.theMastermind
        ]

      lead <- getLead
      beginWithStoryAsset lead Assets.eixodolonsNote

      setAside
        [ Assets.mysteriousSyringe
        , Assets.rotDiagram
        , Assets.hungerDiagram
        , Assets.decayDiagram
        , Enemies.eixodolon
        , Enemies.eixodolonsPet
        , Enemies.facelessAbductor
        , Enemies.facelessAbductor
        ]

      setAside
        [ Locations.labyrinthineHallsFoulSmellingPath
        , Locations.labyrinthineHallsCorpseFilledPath
        , Locations.labyrinthineHallsOvergrownPath
        , Locations.abandonedWarehouse
        ]

      addChaosToken $ groupChaosToken grp
      addChaosToken $ groupChaosToken grp

      case grp of
        GroupA -> do
          setActDeck
            [Acts.sealedInGroupA, Acts.distortionsInTimeGroupA, Acts.theEscapeTheLabyrinthsOfLunacy]
          setAside [Locations.chamberOfDecay]
          secrets <-
            sample
              $ Locations.chamberOfSecretsBloodyPrison
              :| [Locations.chamberOfSecretsMysteriousPrison, Locations.chamberOfSecretsEnshroudedPrison]
          lid <- place secrets
          startAt lid
          assetAt_ Assets.keyOfMysteries lid
        GroupB -> do
          setActDeck
            [Acts.wateryGraveGroupB, Acts.seepingDeathGroupB, Acts.theEscapeTheLabyrinthsOfLunacy]
          setAside [Assets.keyOfMysteries, Locations.chamberOfRot, Locations.chamberOfPoison]
          rain <- place Locations.chamberOfRain
          sorrows <- place Locations.chamberOfSorrows
          reveal rain
          reveal sorrows
          investigators <- allInvestigators
          for_ (nonEmpty investigators) \is -> do
            drowning <- sample is
            push $ PlaceInvestigator drowning (AtLocation rain)
            for_ (filter (/= drowning) investigators) \iid ->
              push $ PlaceInvestigator iid (AtLocation sorrows)
        GroupC -> do
          setActDeck
            [Acts.theLeversGroupC, Acts.thePetGroupC, Acts.theEscapeTheLabyrinthsOfLunacy]
          setAside [Assets.keyOfMysteries, Locations.chamberOfHunger]
          night <- place Locations.chamberOfNight
          place_ Locations.chamberOfRegret
          startAt night
          secrets <-
            sample
              $ Locations.chamberOfSecretsBloodyPrison
              :| [Locations.chamberOfSecretsMysteriousPrison, Locations.chamberOfSecretsEnshroudedPrison]
          placeUnderScenarioReference [secrets]
    ScenarioSpecific "act2Setup" _ -> do
      let meta = toResult attrs.meta
      abductors <- getSetAsideCardsMatching $ cardIs Enemies.facelessAbductor
      shuffleCardsIntoDeck Deck.EncounterDeck abductors
      shuffleEncounterDiscardBackIn
      halls1 <- placeSetAsideLocation Locations.labyrinthineHallsFoulSmellingPath
      halls2 <- placeSetAsideLocation Locations.labyrinthineHallsCorpseFilledPath
      halls3 <- placeSetAsideLocation Locations.labyrinthineHallsOvergrownPath
      push $ SetLocationLabel halls1 "labyrinthineHalls1"
      push $ SetLocationLabel halls2 "labyrinthineHalls2"
      push $ SetLocationLabel halls3 "labyrinthineHalls3"
      case currentGroup meta of
        GroupA -> void $ placeSetAsideLocation Locations.chamberOfDecay
        GroupB -> do
          void $ placeSetAsideLocation Locations.chamberOfRot
          void $ placeSetAsideLocation Locations.chamberOfPoison
        GroupC -> do
          void $ placeSetAsideLocation Locations.chamberOfHunger
          -- Eixodolon's Pet enters play near the Chamber of Hunger, but not
          -- at any location: it is "locked away."
          createSetAsideEnemy_ Enemies.eixodolonsPet Global
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      pure s
    ScenarioSpecific "act3Setup" _ -> do
      warehouse <- placeSetAsideLocation Locations.abandonedWarehouse
      reveal warehouse
      selectEach (AnyEnemy) \eid -> push $ DisengageEnemyFromAll eid
      selectEach UneliminatedInvestigator \iid ->
        push $ PlaceInvestigator iid (AtLocation warehouse)
      createSetAsideEnemy_ Enemies.eixodolon warehouse
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      pure s
    ResolveChaosToken _ Skull iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      whenM (fieldMap InvestigatorClues (> 0) iid) $ placeCluesOnLocation iid Cultist 1
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      loseResources iid Tablet 2
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      randomDiscard iid ElderThing
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull | isHardExpert attrs -> scope "skull" do
          chooseOneM iid do
            labeled' "takeDamage" $ assignDamage iid Skull 1
            labeled' "takeHorror" $ assignHorror iid Skull 1
        Cultist | isEasyStandard attrs -> do
          whenM (fieldMap InvestigatorClues (> 0) iid) $ placeCluesOnLocation iid Cultist 1
        Tablet | isEasyStandard attrs -> loseResources iid Tablet 2
        ElderThing | isEasyStandard attrs -> randomDiscard iid ElderThing
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      let meta = toResult attrs.meta
      let grp = currentGroup meta
      case r of
        NoResolution -> push R1
        Resolution 1 -> do
          let meta' = meta {playedGroups = grp : playedGroups meta}
          push $ SetScenarioMeta $ toJSON meta'
          if length (playedGroups meta') >= 3
            then do_ (ScenarioResolutionStep 1 (Resolution 1))
            else do
              resolutionWithChooseOne "resolution1" do
                labeled' "endScenario" do
                  killRemainingInvestigators attrs
                  push GameOver
                  endOfScenario
                labeled' "playAnotherGroup" do
                  killRemainingInvestigators attrs
                  push $ ScenarioResolutionStep 10 (Resolution 1)
        Resolution 2 -> do
          let meta' =
                meta
                  { playedGroups = grp : playedGroups meta
                  , survivedGroups = grp : survivedGroups meta
                  }
          push $ SetScenarioMeta $ toJSON meta'
          if length (playedGroups meta') >= 3
            then do_ (ScenarioResolutionStep 1 (Resolution 2))
            else do
              resolutionWithChooseOne "resolution2" do
                labeled' "endScenario" endOfScenario
                labeled' "playAnotherGroup" $ push $ ScenarioResolutionStep 10 (Resolution 2)
        _ -> error "Invalid resolution"
      pure s
    Do (ScenarioResolutionStep 1 (Resolution n)) -> scope "resolutions" do
      -- Mini-campaign over: the resolution depends on how many groups
      -- escaped the labyrinth.
      let meta = toResult attrs.meta
      when (n == 1) $ killRemainingInvestigators attrs
      case length (survivedGroups meta) of
        0 -> do
          resolution "resolution1"
          push GameOver
        1 -> resolution "resolution2"
        2 -> resolution "resolution3"
        _ -> resolution "resolution4"
      endOfScenario
      pure s
    ScenarioResolutionStep 10 _ -> do
      standalone <- getIsStandalone
      pushAll
        $ [ResetGame]
        <> [StandaloneSetup | standalone]
        <> [ PreScenarioSetup
           , ChooseLeadInvestigator
           , SetPlayerOrder
           , SetupInvestigators
           , InvestigatorsMulligan
           , Setup
           , EndSetup
           ]
      let resetAttrs = toAttrs $ theLabyrinthsOfLunacy attrs.difficulty
      pure
        . TheLabyrinthsOfLunacy
        $ resetAttrs
          { scenarioTimesPlayed = scenarioTimesPlayed attrs + 1
          , scenarioPlayerDecks = scenarioPlayerDecks attrs
          , scenarioStoryCards = scenarioStoryCards attrs
          , scenarioMeta = attrs.meta
          }
    _ -> TheLabyrinthsOfLunacy <$> liftRunMessage msg attrs

killRemainingInvestigators :: ReverseQueue m => ScenarioAttrs -> m ()
killRemainingInvestigators attrs =
  selectEach UneliminatedInvestigator \iid ->
    push $ InvestigatorKilled (toSource attrs) iid
