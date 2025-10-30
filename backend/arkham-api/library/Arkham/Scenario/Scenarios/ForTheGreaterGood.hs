module Arkham.Scenario.Scenarios.ForTheGreaterGood (setupForTheGreaterGood, forTheGreaterGood, ForTheGreaterGood (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep
import Arkham.Campaigns.TheCircleUndone.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorKeys))
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Xp

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

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

setupForTheGreaterGood
  :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupForTheGreaterGood _attrs = do
  setup $ ul do
    li "gatherSets"
    scope "membersOfTheLodge" $ li.nested "instructions" do
      li "actDeck"
      li "weveBeenExpectingYou"
      li "removeFromGame"
    scope "otherwise" $ li.nested "instructions" do
      li "actDeck"
      li "membersOnly"
      li "removeFromGame"
    li "placeLocations"
    li "setAside"
    li "setAsideKeys"
    unscoped $ li "shuffleRemainder"

  scope "keys" $ flavor $ h.noUnderline "title" >> p "body"

  whenReturnTo $ gather Set.ReturnToForTheGreaterGood
  gather Set.ForTheGreaterGood
  gather Set.CityOfSins `orWhenReturnTo` gather Set.CityOfTheDamned
  gather Set.SilverTwilightLodge
  gather Set.AncientEvils `orWhenReturnTo` gather Set.ImpendingEvils
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

  lounge <- Locations.lounge `orSampleIfReturnTo` [Locations.returnToLounge]
  placeAll [Locations.lodgeCatacombs, lounge]

  isReturnTo <- getIsReturnTo
  setAside
    $ [ Locations.library
      , Locations.vault
      , Locations.innerSanctum
      , Locations.sanctumDoorwayHoldingCells
      , Locations.sanctumDoorwayCeremonyRoom
      , Assets.puzzleBox
      , Enemies.summonedBeast
      , Assets.augustLindquist
      , if membersOfTheLodge then Enemies.nathanWickMasterOfInitiation else Enemies.nathanWickMasterOfIndoctrination
      ]
    <> (guard isReturnTo *> [Locations.relicStorage, Locations.shroudedArchive])

  skullKey <- toKey <$> createChaosToken #skull
  cultistKey <- toKey <$> createChaosToken #cultist
  tabletKey <- toKey <$> createChaosToken #tablet
  elderThingKey <- toKey <$> createChaosToken #elderthing

  setAsideKeys [skullKey, cultistKey, tabletKey, elderThingKey]

  whenReturnTo $ addAdditionalReferences ["54042b"]

instance RunMessage ForTheGreaterGood where
  runMessage msg s@(ForTheGreaterGood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
      learnedNothing <- getHasRecord TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
      neverSeenOrHeardFromAgain <- getHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain

      flavor do
        setTitle "title"
        p "intro1"
        ul do
          li.validate membersOfTheLodge "membersOfTheLodge"
          li.validate enemiesOfTheLodge "enemiesOfTheLodge"
          li.validate learnedNothing "learnedNothingOfTheLodgesSchemes"
          li.validate neverSeenOrHeardFromAgain "neverSeenOrHeardFromAgain"

      if
        | membersOfTheLodge -> doStep 2 PreScenarioSetup
        | enemiesOfTheLodge -> doStep 3 PreScenarioSetup
        | learnedNothing -> doStep 4 PreScenarioSetup
        | otherwise -> doStep 5 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      showSidebar <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      flavor do
        setTitle "title"
        p "intro2"
        p.validate showSidebar "deceivingTheLodge"
        p "intro2Skip"
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro5"
      pure s
    StandaloneSetup -> scope "standalone" do
      setChaosTokens standaloneChaosTokens

      leadChooseOneM do
        labeled' "members" $ record TheInvestigatorsAreMembersOfTheLodge
        labeled' "notMembers" nothing
      pure s
    Setup -> runScenarioSetup ForTheGreaterGood attrs $ setupForTheGreaterGood attrs
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
          closestCultists <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Trait.Cultist
          chooseTargetM iid closestCultists \cultist -> placeDoom Cultist cultist 1
        else do
          cultists <- select $ EnemyWithTrait Trait.Cultist
          for_ cultists \cultist -> placeDoom Cultist cultist 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      if isEasyStandard attrs
        then do
          closestCultists <- select $ NearestEnemyToFallback iid #cultist <> EnemyWithAnyDoom
          chooseTargetM iid closestCultists \cultist -> do
            removeDoom ElderThing cultist 1
            placeDoomOnAgenda 1
        else do
          maxDoomCultists <- selectMax EnemyDoom (EnemyWithTrait Trait.Cultist)
          if notNull maxDoomCultists
            then do
              agenda <- getCurrentAgenda
              maxDoom <- fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)
              chooseTargetM iid maxDoomCultists \cultist -> do
                removeAllDoom ElderThing cultist
                placeTokens ElderThing agenda Doom maxDoom
            else drawAnotherChaosToken iid
      pure s
    ScenarioResolution n -> scope "resolutions" do
      iids <- allInvestigators
      let isNumberChaosTokenKey = \case
            TokenKey chaosToken -> isNumberChaosToken chaosToken.face
            _ -> False
      ks <-
        concatMap (filter isNumberChaosTokenKey . toList)
          <$> selectField InvestigatorKeys (not_ DefeatedInvestigator)

      let updateTotal = bimap (+ length ks) (map (second (+ length ks)))
      let recordTokens (XpBreakdown inner) =
            XpBreakdown
              $ inner
              <> map
                ( \k ->
                    AllGainXp (XpDetail XpFromVictoryDisplay (withVar "key" (String $ keyName k) $ ikey' "extraKey") 1)
                )
                ks
      case n of
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXpEdit' attrs recordTokens updateTotal
          record TheGuardianOfTheTrapEmerged
          endOfScenario
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpEdit' attrs recordTokens updateTotal
          record TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
          addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.puzzleBox
          endOfScenarioThen (InterludeStep 3 Nothing)
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpEdit' attrs recordTokens updateTotal
          record TheInvestigatorsDiscoveredHowToOpenThePuzzleBox
          addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.puzzleBox
          endOfScenario
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXpEdit' attrs recordTokens updateTotal
          record TheGuardianOfTheTrapEmergedAndWasDefeated
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> ForTheGreaterGood <$> liftRunMessage msg attrs
