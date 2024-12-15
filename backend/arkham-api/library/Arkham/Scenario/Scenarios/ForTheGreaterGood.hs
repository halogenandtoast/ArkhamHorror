module Arkham.Scenario.Scenarios.ForTheGreaterGood (ForTheGreaterGood (..), forTheGreaterGood) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers.Agenda
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ForTheGreaterGood.Story
import Arkham.Token
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
  runMessage msg s@(ForTheGreaterGood attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro1

      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      if membersOfTheLodge
        then doStep 2 PreScenarioSetup
        else do
          enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
          if enemiesOfTheLodge
            then doStep 3 PreScenarioSetup
            else do
              learnedNothing <- getHasRecord TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
              if learnedNothing
                then doStep 4 PreScenarioSetup
                else doStep 5 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> do
      showSidebar <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      let convert = if showSidebar then addFlavorEntry decievingTheLodge else id
      story $ convert intro2
      pure s
    DoStep 3 PreScenarioSetup -> do
      story intro3
      pure s
    DoStep 4 PreScenarioSetup -> do
      story intro4
      pure s
    DoStep 5 PreScenarioSetup -> do
      story intro5
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens

      lead <- getLead
      chooseOneM lead do
        labeled "The investigators are members of the Lodge." $ record TheInvestigatorsAreMembersOfTheLodge
        labeled "The investigators are not members of the Lodge." nothing
      pure s
    Setup -> runScenarioSetup ForTheGreaterGood attrs do
      gather Set.ForTheGreaterGood
      gather Set.CityOfSins
      gather Set.SilverTwilightLodge
      gather Set.AncientEvils
      gather Set.DarkCult
      gather Set.LockedDoors

      setAgendaDeck [Agendas.theHierophantV, Agendas.endsAndMeans]

      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      let act1 = if membersOfTheLodge then Acts.warmWelcome else Acts.infiltratingTheLodge
      setActDeck [act1, Acts.obtainingTheDevice, Acts.theFourKeys]

      if membersOfTheLodge
        then do
          startAt =<< place Locations.lodgeGatesWeveBeenExpectingYou
          placeAll [Locations.lobbyWeveBeenExpectingYou, Locations.lodgeCellarWeveBeenExpectingYou]
          removeOneOfEach
            [ Locations.lodgeGatesMembersOnly
            , Locations.lobbyMembersOnly
            , Locations.lodgeCellarMembersOnly
            , Enemies.acolyte
            , Enemies.acolyte
            , Enemies.acolyte
            , Enemies.wizardOfTheOrder
            , Enemies.knightOfTheInnerCircle
            , Enemies.knightOfTheInnerCircle
            , Enemies.cellKeeper
            ]
        else do
          startAt =<< place Locations.lodgeGatesMembersOnly
          placeAll [Locations.lobbyMembersOnly, Locations.lodgeCellarMembersOnly]
          removeOneOfEach
            [ Locations.lodgeGatesWeveBeenExpectingYou
            , Locations.lobbyWeveBeenExpectingYou
            , Locations.lodgeCellarWeveBeenExpectingYou
            , Enemies.lodgeNeophyte
            , Enemies.lodgeNeophyte
            , Enemies.lodgeNeophyte
            , Enemies.keeperOfSecrets
            , Enemies.knightOfTheOuterVoid
            , Enemies.knightOfTheOuterVoid
            , Enemies.lodgeJailor
            ]

      placeAll [Locations.lodgeCatacombs, Locations.lounge]

      setAside
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

      setAsideKeys [SkullKey, CultistKey, TabletKey, ElderThingKey]
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      noCultists <- selectNone $ EnemyWithTrait Trait.Cultist
      when noCultists $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      if isEasyStandard attrs
        then do
          closestCultists <- select $ NearestEnemyTo iid $ EnemyWithTrait Trait.Cultist
          chooseTargetM iid closestCultists \cultist -> placeDoom Cultist cultist 1
        else do
          cultists <- select $ EnemyWithTrait Trait.Cultist
          for_ cultists \cultist -> placeDoom Cultist cultist 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      if isEasyStandard attrs
        then do
          closestCultists <- select $ NearestEnemyTo iid (EnemyWithTrait Trait.Cultist) <> EnemyWithAnyDoom
          chooseTargetM iid closestCultists \cultist -> do
            removeDoom ElderThing cultist 1
            placeDoomOnAgenda 1
        else do
          maxDoomCultists <- selectMax EnemyDoom (EnemyWithTrait Trait.Cultist)
          if notNull maxDoomCultists
            then do
              agenda <- getCurrentAgenda
              maxDoom <- fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)
              chooseOrRunOne
                iid
                [ targetLabel
                  cultist
                  [ RemoveAllDoom (toSource attrs) (toTarget cultist)
                  , PlaceTokens (ChaosTokenEffectSource ElderThing) (toTarget agenda) Doom maxDoom
                  ]
                | cultist <- maxDoomCultists
                ]
            else drawAnotherChaosToken iid
      pure s
    ScenarioResolution n -> do
      iids <- allInvestigators
      case n of
        NoResolution -> do
          story noResolution
          record TheGuardianOfTheTrapEmerged
          allGainXp attrs
          endOfScenario
        Resolution 1 -> do
          story resolution1
          record TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
          addCampaignCardToDeckChoice iids Assets.puzzleBox
          allGainXp attrs
          endOfScenarioThen (InterludeStep 3 Nothing)
        Resolution 2 -> do
          story resolution2
          record TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
          addCampaignCardToDeckChoice iids Assets.puzzleBox
          allGainXp attrs
          endOfScenario
        Resolution 3 -> do
          story resolution3
          record TheGuardianOfTheTrapEmergedAndWasDefeated
          allGainXp attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> ForTheGreaterGood <$> liftRunMessage msg attrs
