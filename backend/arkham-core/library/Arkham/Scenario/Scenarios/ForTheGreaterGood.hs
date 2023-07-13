module Arkham.Scenario.Scenarios.ForTheGreaterGood (
  ForTheGreaterGood (..),
  forTheGreaterGood,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers.Agenda
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.ForTheGreaterGood.Story
import Arkham.Trait qualified as Trait

newtype ForTheGreaterGood = ForTheGreaterGood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forTheGreaterGood :: Difficulty -> ForTheGreaterGood
forTheGreaterGood difficulty =
  scenario
    ForTheGreaterGood
    "05197"
    "For the Greater Good"
    difficulty
    [ ".       .      lodgeGates .              ."
    , ".       lobby  .          lodgeCellar    ."
    , "library lounge .          lodgeCatacombs sanctumDoorway1"
    , ".       vault  .          innerSanctum   sanctumDoorway2"
    ]

instance HasChaosTokenValue ForTheGreaterGood where
  getChaosTokenValue iid chaosTokenFace (ForTheGreaterGood attrs) = case chaosTokenFace of
    Skull -> do
      doomValue <-
        if isEasyStandard attrs
          then selectAgg' Max0 EnemyDoom (EnemyWithTrait Trait.Cultist)
          else selectAgg' Sum EnemyDoom (EnemyWithTrait Trait.Cultist)
      pure $ ChaosTokenValue Cultist (NegativeModifier doomValue)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 3)
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
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage ForTheGreaterGood where
  runMessage msg s@(ForTheGreaterGood attrs) = case msg of
    PreScenarioSetup -> do
      iids <- allInvestigatorIds
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
      showSidebar <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      learnedNothing <- getHasRecord TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain

      let intro2Sidebar = if showSidebar then decievingTheLodge else mempty

      pushAll $
        story iids intro1
          : [story iids (intro2 <> intro2Sidebar) | membersOfTheLodge]
            <> [story iids intro3 | not membersOfTheLodge, enemiesOfTheLodge]
            <> [story iids intro4 | not membersOfTheLodge, not enemiesOfTheLodge, learnedNothing]
            <> [ story iids intro5
               | not membersOfTheLodge
               , not enemiesOfTheLodge
               , not learnedNothing
               , neverSeenOrHeardFromAgain
               ]
      pure s
    StandaloneSetup -> do
      lead <- getLead
      push $
        chooseOne
          lead
          [ Label "The investigators are members of the Lodge." [Record TheInvestigatorsAreMembersOfTheLodge]
          , Label "The investigators are not members of the Lodge." []
          ]
      pure s
    SetChaosTokensForScenario -> do
      pushWhenM getIsStandalone (SetChaosTokens standaloneChaosTokens)
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeck
          [ EncounterSet.ForTheGreaterGood
          , EncounterSet.CityOfSins
          , EncounterSet.SilverTwilightLodge
          , EncounterSet.AncientEvils
          , EncounterSet.DarkCult
          , EncounterSet.LockedDoors
          ]
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge

      let
        removals =
          if membersOfTheLodge
            then
              [ Enemies.acolyte
              , Enemies.acolyte
              , Enemies.acolyte
              , Enemies.wizardOfTheOrder
              , Enemies.knightOfTheInnerCircle
              , Enemies.knightOfTheInnerCircle
              , Enemies.cellKeeper
              ]
            else
              [ Enemies.lodgeNeophyte
              , Enemies.lodgeNeophyte
              , Enemies.lodgeNeophyte
              , Enemies.keeperOfSecrets
              , Enemies.knightOfTheOuterVoid
              , Enemies.knightOfTheOuterVoid
              , Enemies.lodgeJailor
              ]
        encounterDeck' = removeEachFromDeck encounterDeck removals
        act1 = if membersOfTheLodge then Acts.warmWelcome else Acts.infiltratingTheLodge
        lodgeGates =
          if membersOfTheLodge
            then Locations.lodgeGatesWeveBeenExpectingYou
            else Locations.lodgeGatesMembersOnly
        lobby = if membersOfTheLodge then Locations.lobbyWeveBeenExpectingYou else Locations.lobbyMembersOnly
        lodgeCellar =
          if membersOfTheLodge
            then Locations.lodgeCellarWeveBeenExpectingYou
            else Locations.lodgeCellarMembersOnly

      (lodgeGatesId, placeLodgeGates) <- placeLocationCard lodgeGates
      otherPlacements <-
        placeLocationCards_ [lobby, lodgeCellar, Locations.lodgeCatacombs, Locations.lounge]

      pushAll $
        [ SetEncounterDeck encounterDeck'
        , SetAgendaDeck
        , SetActDeck
        , placeLodgeGates
        , MoveAllTo (toSource attrs) lodgeGatesId
        ]
          <> otherPlacements

      agendas <- genCards [Agendas.theHierophantV, Agendas.endsAndMeans]
      acts <- genCards [act1, Acts.obtainingTheDevice, Acts.theFourKeys]

      setAsideCards <-
        genCards
          [ Locations.library
          , Locations.vault
          , Locations.innerSanctum
          , Locations.sanctumDoorwayHoldingCells
          , Locations.sanctumDoorwayCeremonyRoom
          , Assets.puzzleBox
          , Enemies.summonedBeast
          , Assets.augustLindquist
          , Enemies.nathanWickMasterOfInitiation
          ]

      ForTheGreaterGood
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (setAsideKeysL .~ setFromList [SkullKey, CultistKey, TabletKey, ElderThingKey])
          )
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      noCultists <- selectNone $ EnemyWithTrait Trait.Cultist
      pushWhen noCultists $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      if isEasyStandard attrs
        then do
          closestCultists <- selectList $ NearestEnemy $ EnemyWithTrait Trait.Cultist
          unless (null closestCultists) $
            push $
              chooseOrRunOne
                iid
                [ targetLabel cultist [PlaceDoom (ChaosTokenEffectSource Cultist) (toTarget cultist) 1]
                | cultist <- closestCultists
                ]
        else do
          cultists <- selectList $ EnemyWithTrait Trait.Cultist
          pushAll [PlaceDoom (ChaosTokenEffectSource Cultist) (toTarget cultist) 1 | cultist <- cultists]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      if isEasyStandard attrs
        then do
          closestCultists <- selectList $ NearestEnemy (EnemyWithTrait Trait.Cultist) <> EnemyWithAnyDoom
          unless (null closestCultists) $
            push $
              chooseOne
                iid
                [ targetLabel
                  cultist
                  [RemoveDoom (ChaosTokenEffectSource ElderThing) (toTarget cultist) 1, PlaceDoomOnAgenda]
                | cultist <- closestCultists
                ]
        else do
          maxDoomCultists <- selectMax EnemyDoom (EnemyWithTrait Trait.Cultist)
          if notNull maxDoomCultists
            then do
              agenda <- getCurrentAgenda
              maxDoom <- fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)
              push $
                chooseOrRunOne
                  iid
                  [ targetLabel
                    cultist
                    [ RemoveAllDoom (toSource attrs) (toTarget cultist)
                    , PlaceDoom (ChaosTokenEffectSource ElderThing) (toTarget agenda) maxDoom
                    ]
                  | cultist <- maxDoomCultists
                  ]
            else push $ DrawAnotherChaosToken iid
      pure s
    ScenarioResolution n -> do
      iids <- allInvestigatorIds
      lead <- getLead
      xp <- toGainXp attrs getXp
      case n of
        NoResolution ->
          pushAll $ [story iids noResolution, Record TheGuardianOfTheTrapEmerged] <> xp <> [EndOfGame Nothing]
        Resolution 1 ->
          pushAll $
            [ story iids resolution1
            , Record TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
            , addCampaignCardToDeckChoice
                lead
                iids
                Assets.puzzleBox
            ]
              <> xp
              <> [EndOfGame (Just $ UpgradeDeckStep $ InterludeStep 3 Nothing)]
        Resolution 2 ->
          pushAll $
            [ story iids resolution2
            , Record TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
            , addCampaignCardToDeckChoice
                lead
                iids
                Assets.puzzleBox
            ]
              <> xp
              <> [EndOfGame Nothing]
        Resolution 3 ->
          pushAll $
            [ story iids resolution3
            , Record TheGuardianOfTheTrapEmergedAndWasDefeated
            ]
              <> xp
              <> [EndOfGame Nothing]
        _ -> error "invalid resolution"
      pure s
    _ -> ForTheGreaterGood <$> runMessage msg attrs
