module Arkham.Scenario.Scenarios.TheLabyrinthsOfLunacy (theLabyrinthsOfLunacy) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Field (AgendaDoom, AgendaDoomThreshold))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option (CampaignOption (PlayAsMiniCampaign))
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id (getId)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (CanAdvance (CanAdvance), chooseDecks)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record)
import Arkham.Phase (MythosPhaseStep (PlaceDoomOnAgendaStep))
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (ScenarioAttrs (..))
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Key qualified as Log
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Meta

newtype TheLabyrinthsOfLunacy = TheLabyrinthsOfLunacy ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLabyrinthsOfLunacy :: Difficulty -> TheLabyrinthsOfLunacy
theLabyrinthsOfLunacy = sideStory_ TheLabyrinthsOfLunacy "70001" "The Labyrinths of Lunacy"

{- FOURMOLU_DISABLE -}
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
    -- Standalone vs. mini-campaign is chosen on the new-game screen and arrives as
    -- a campaign option; record it in the meta before the scenario is set up.
    HandleOption PlayAsMiniCampaign -> do
      whenM getIsStandalone do
        setScenarioMeta $ (toResultDefault (initialMeta GroupA) attrs.meta) {miniCampaign = True}
      pure s
    PreScenarioSetup -> scope "intro" do
      -- The mode is already set (from the new-game option, or defaults to a single
      -- standalone game); just pick which as-yet-unplayed group is trapped here.
      chooseGroup (toResultDefault (initialMeta GroupA) attrs.meta)
      pure s
    StandaloneSetup -> do
      setChaosTokens $ if isEasyStandard attrs then standardTokens else hardTokens
      pure s
    Setup -> runScenarioSetup TheLabyrinthsOfLunacy attrs do
      grp <- getGroup
      setup $ ul do
        li "gatherSets"
        li.nested "singleGroup" do
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
          setLayout
            [ ".                  chamberOfDecay     abandonedWarehouse"
            , ".                  labyrinthineHalls1 ."
            , ".                  chamberOfSecrets   ."
            , "labyrinthineHalls2 .                  labyrinthineHalls3"
            ]
          setActDeck [Acts.sealedInGroupA, Acts.distortionsInTimeGroupA, Acts.theEscapeTheLabyrinthsOfLunacy]
          setAside [Locations.chamberOfDecay]
          secrets <-
            sample
              $ Locations.chamberOfSecretsBloodyPrison
              :| [Locations.chamberOfSecretsMysteriousPrison, Locations.chamberOfSecretsEnshroudedPrison]
          lid <- place secrets
          startAt lid
          assetAt_ Assets.keyOfMysteries lid
        GroupB -> do
          setLayout
            [ ".                  chamberOfRain      chamberOfSecrets   chamberOfNight"
            , ".                  chamberOfSorrows   .                  chamberOfRegret"
            , "labyrinthineHalls1 labyrinthineHalls2 labyrinthineHalls3 ."
            , "chamberOfDecay     chamberOfRot       chamberOfHunger    ."
            , ".                  chamberOfPoison    .                  ."
            , ".                  abandonedWarehouse .                  ."
            ]
          setActDeck [Acts.wateryGraveGroupB, Acts.seepingDeathGroupB, Acts.theEscapeTheLabyrinthsOfLunacy]
          setAside [Assets.keyOfMysteries, Locations.chamberOfRot, Locations.chamberOfPoison]
          rain <- place Locations.chamberOfRain
          sorrows <- place Locations.chamberOfSorrows
          reveal rain
          reveal sorrows
          investigators <- allInvestigators
          for_ (nonEmpty investigators) \is -> do
            (drowning, rest) <- sampleWithRest is
            push $ PlaceInvestigator drowning (AtLocation rain)
            for_ rest \iid ->
              push $ PlaceInvestigator iid (AtLocation sorrows)
        GroupC -> do
          setLayout
            [ ".                  chamberOfRegret    ."
            , ".                  chamberOfNight     ."
            , ".                  labyrinthineHalls1 ."
            , ".                  chamberOfHunger    ."
            , ".                  abandonedWarehouse ."
            , "labyrinthineHalls2 .                  labyrinthineHalls3"
            ]
          setActDeck [Acts.theLeversGroupC, Acts.thePetGroupC, Acts.theEscapeTheLabyrinthsOfLunacy]
          setAside [Assets.keyOfMysteries, Locations.chamberOfHunger]
          night <- place Locations.chamberOfNight
          place_ Locations.chamberOfRegret
          startAt night
          secrets <-
            sample
              $ Locations.chamberOfSecretsBloodyPrison
              :| [Locations.chamberOfSecretsMysteriousPrison, Locations.chamberOfSecretsEnshroudedPrison]
          placeUnderScenarioReference [secrets]
    PlaceDoomOnAgenda n canAdvance -> do
      agendaId <- selectJust UnflippedAgenda
      doom <- field AgendaDoom agendaId
      threshold <- traverse getGameValue =<< field AgendaDoomThreshold agendaId
      mythosStep <- getCurrentMythosPhaseStep
      lead <- getLead
      let placeAgendaDoom :: ReverseQueue m => Int -> m ()
          placeAgendaDoom amount = do
            placeDoom attrs agendaId amount
            when (canAdvance == CanAdvance) $ forTarget agendaId AdvanceAgendaIfThresholdSatisfied
      if mythosStep == Just PlaceDoomOnAgendaStep
        then chooseOneM lead do
          withI18n $ countVar n $ labeled' "placeAgendaDoom" $ placeAgendaDoom n
          labeled' "satisfyDoomThreshold"
            $ placeAgendaDoom
            $ maybe n (\value -> max 0 $ value - doom) threshold
        else placeAgendaDoom n
      pure s
    ScenarioSpecific "act2Setup" _ -> scope "act2Setup" do
      grp <- getGroup
      flavor do
        h "title"
        ul do
          li "shuffleEncounterDeck"
          li "labyrinthineHalls"
          case grp of
            GroupA -> li "chamberOfDecay"
            GroupB -> li "chambersOfRotAndPoison"
            GroupC -> do
              li "chamberOfHunger"
              li "eixodolonsPet"
          li "removeDoom"
      shuffleSetAsideIntoDeck Deck.EncounterDeck (cardIs Enemies.facelessAbductor)
      shuffleEncounterDiscardBackIn

      placeRandomLocationGroupCards
        "labyrinthineHalls"
        [ Locations.labyrinthineHallsFoulSmellingPath
        , Locations.labyrinthineHallsCorpseFilledPath
        , Locations.labyrinthineHallsOvergrownPath
        ]
      case grp of
        GroupA -> placeSetAsideLocation_ Locations.chamberOfDecay
        GroupB -> do
          placeSetAsideLocation_ Locations.chamberOfRot
          placeSetAsideLocation_ Locations.chamberOfPoison
        GroupC -> do
          placeSetAsideLocation_ Locations.chamberOfHunger
          -- Eixodolon's Pet enters play near the Chamber of Hunger, but not
          -- at any location: it is "locked away."
          createSetAsideEnemy_ Enemies.eixodolonsPet Global
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      pure s
    ScenarioSpecific "act3Setup" _ -> scope "act3Setup" do
      flavor do
        h "title"
        ul do
          li "abandonedWarehouse"
          li "moveInvestigators"
          li "eixodolon"
          li "removeDoom"
      warehouse <- placeSetAsideLocation Locations.abandonedWarehouse
      reveal warehouse
      selectEach AnyEnemy disengageEnemyFromAll
      selectEach UneliminatedInvestigator \iid ->
        push $ PlaceInvestigator iid (AtLocation warehouse)
      createSetAsideEnemy_ Enemies.eixodolon warehouse
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      pure s
    ResolveChaosToken _ Skull iid -> do
      drawAnotherChaosToken iid
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
          chooseOneM iid $ unscoped $ countVar 1 do
            labeled' "takeDamage" $ assignDamage iid Skull 1
            labeled' "takeHorror" $ assignHorror iid Skull 1
        Cultist | isEasyStandard attrs -> do
          whenM (fieldMap InvestigatorClues (> 0) iid) $ placeCluesOnLocation iid Cultist 1
        Tablet | isEasyStandard attrs -> loseResources iid Tablet 2
        ElderThing | isEasyStandard attrs -> randomDiscard iid ElderThing
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R1
        Resolution 1 -> resolveGroup attrs Log.TheGroupPerished
        Resolution 2 -> resolveGroup attrs Log.TheGroupEscapedTheLabyrinth
        _ -> error "Invalid resolution"
      pure s
    ScenarioResolutionStep 10 _ -> do
      players <- allPlayers
      batchId <- getId
      -- Reset the board before chooseDecks parks the queue waiting for new decks.
      pushAll
        [ ResetGame
        , chooseDecks batchId players [ResetInvestigators, ResetGame, StartScenario attrs.id Nothing]
        ]
      pure $ TheLabyrinthsOfLunacy attrs {scenarioPlayerDecks = mempty, scenarioStoryCards = mempty}
    _ -> TheLabyrinthsOfLunacy <$> liftRunMessage msg attrs

resolveGroup :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> Log.GroupOutcome -> m ()
resolveGroup attrs outcome = do
  meta <- getMeta
  let survived = outcome == Log.TheGroupEscapedTheLabyrinth
  let meta' = completeCurrentGroup survived meta
  setScenarioMeta meta'
  recordGroupOutcome meta.currentGroup outcome
  unless survived $ selectEach UneliminatedInvestigator $ push . InvestigatorKilled (toSource attrs)
  if miniCampaign meta' && not (miniCampaignComplete meta')
    then resolutionWithChooseOne (resolutionKey meta') do
      labeled' "endScenario" do
        when (null meta'.survivedGroups) $ push GameOver
        endOfScenario
      labeled' "playAnotherGroup" $ push $ ScenarioResolutionStep 10 (Resolution 1)
    else do
      resolution $ resolutionKey meta'
      when (null meta'.survivedGroups) $ push GameOver
      endOfScenario

recordGroupOutcome :: ReverseQueue m => Group -> Log.GroupOutcome -> m ()
recordGroupOutcome grp outcome = record $ case grp of
  GroupA -> Log.GroupA outcome
  GroupB -> Log.GroupB outcome
  GroupC -> Log.GroupC outcome

chooseGroup :: (HasI18n, ReverseQueue m) => Meta -> m ()
chooseGroup meta = do
  let remaining = remainingGroups meta
  let chooseGroupTxt = if miniCampaign meta then "chooseGroup" else "chooseGroupSingle"
  storyWithChooseOneM' (h "title" >> p chooseGroupTxt) do
    for_ remaining \g -> do
      popScope $ labeled' (groupLabel g) do
        push $ SetScenarioMeta $ toJSON meta {currentGroup = g}
        scope "intro" $ flavor do
          h "title"
          p $ case g of
            GroupA -> "groupA"
            GroupB -> "groupB"
            GroupC -> "groupC"
          p "note"
