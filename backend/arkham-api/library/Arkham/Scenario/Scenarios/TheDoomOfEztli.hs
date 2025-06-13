module Arkham.Scenario.Scenarios.TheDoomOfEztli (theDoomOfEztli, TheDoomOfEztli (..), setupTheDoomOfEztli) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Campaigns.TheForgottenAge.Meta qualified as CampaignMeta
import Arkham.Card.CardCode
import Arkham.Effect.Window
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types hiding (metaL)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Campaign
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types hiding (metaL)
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage)
import Arkham.Scenarios.TheDoomOfEztli.Helpers
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window

newtype TheDoomOfEztli = TheDoomOfEztli ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The Doom of Eztli

For the layout Magic and Science will cause the layout to switch to using
the bottom line
-}
theDoomOfEztli :: Difficulty -> TheDoomOfEztli
theDoomOfEztli difficulty =
  scenario
    TheDoomOfEztli
    "04054"
    "The Doom of Eztli"
    difficulty
    [ ".        ancientHall  undergroundRuins .             .             .    ."
    , "entryway ancientHall  undergroundRuins secretPassage chamberOfTime .    ."
    , "entryway grandChamber burialPit        secretPassage chamberOfTime .    ."
    , ".        grandChamber burialPit        .             .             .    ."
    , "pos1     pos2         pos3             pos4          pos5          pos6 pos7"
    ]

instance HasChaosTokenValue TheDoomOfEztli where
  getChaosTokenValue iid chaosTokenFace (TheDoomOfEztli attrs) =
    case chaosTokenFace of
      Skull -> do
        hasDoom <- selectAny $ LocationWithAnyDoom <> locationWithInvestigator iid
        pure
          $ if hasDoom
            then toChaosTokenValue attrs Skull 3 4
            else toChaosTokenValue attrs Skull 1 2
      face | face `elem` [Cultist, Tablet] -> do
        n <-
          if isEasyStandard attrs
            then selectCount LocationWithAnyDoom
            else getSum <$> selectAgg Sum LocationDoom LocationWithAnyDoom
        pure $ ChaosTokenValue Cultist (NegativeModifier n)
      ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
      otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecorded = setFromList [toCampaignLogKey TheInvestigatorsClearedAPathToTheEztliRuins]
    }

setupTheDoomOfEztli
  :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheDoomOfEztli attrs = do
  setup do
    ul do
      li "gatherSets"
      li "placeLocations"
      li "explorationDeck"
      li "setAside"
      li "poisoned"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToTheDoomOfEztli
  gather Set.TheDoomOfEztli
  gather Set.AgentsOfYig
  gather Set.YigsVenom `orWhenReturnTo` gather Set.VenomousHate
  gather Set.TemporalFlux `orWhenReturnTo` gather Set.TemporalHunters
  gather Set.DeadlyTraps
  gather Set.ForgottenRuins
  gather Set.Poison
  gather Set.ChillingCold

  entryway <- place Locations.entryway `orWhenReturnTo` place Locations.entrywayRearrangedByTime
  let resolution4Count = toResultDefault 0 attrs.meta
  when (resolution4Count > 0) $ placeDoom attrs entryway resolution4Count
  startAt entryway

  isReturnTo <- getIsReturnTo
  setAsidePoisonedCount <- getSetAsidePoisonedCount
  setAside
    $ [ if isReturnTo then Locations.chamberOfTimeRearrangedByTime else Locations.chamberOfTime
      , Assets.relicOfAgesADeviceOfSomeSort
      , if isReturnTo then Enemies.harbingerOfValusiaTheSleeperReturns else Enemies.harbingerOfValusia
      ]
    <> replicate setAsidePoisonedCount Treacheries.poisoned

  whenReturnTo $ setAside $ replicate 3 Enemies.pitViper

  setActDeck [Acts.intoTheRuins, Acts.magicAndScience, Acts.escapeTheRuins]
  setAgendaDeck [Agendas.somethingStirs, Agendas.theTempleWarden]

  let
    explorationDeck =
      if isReturnTo
        then
          [ Locations.sealedPassage
          , Locations.mosaicChamber
          , Locations.tombOfTheAncients
          , Locations.throneRoom
          , Locations.snakePit
          , Locations.ancientHallRearrangedByTime
          , Locations.grandChamberRearrangedByTime
          ]
        else
          [ Locations.ancientHall
          , Locations.grandChamber
          , Locations.burialPit
          , Locations.undergroundRuins
          , Locations.secretPassage
          , Treacheries.illOmen
          , Treacheries.deepDark
          , Treacheries.finalMistake
          , Treacheries.entombed
          , Treacheries.cryptChill
          ]

  addExtraDeck ExplorationDeck =<< shuffle explorationDeck

  whenReturnTo do
    removeEvery
      [ Locations.burialPit
      , Locations.undergroundRuins
      , Locations.secretPassage
      , Locations.entryway
      , Locations.chamberOfTime
      , Locations.ancientHall
      , Locations.grandChamber
      , Enemies.harbingerOfValusia
      ]
    addAdditionalReferences ["53017b"]
    createAbilityEffect EffectGameWindow
      $ mkAbility (SourceableWithCardCode (CardCode "53016b") ScenarioSource) 1
      $ forced
      $ Explored #after Anyone Anywhere (SuccessfulExplore Anywhere)

instance RunMessage TheDoomOfEztli where
  runMessage msg s@(TheDoomOfEztli attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      forcedToWait <- getHasRecord TheInvestigatorsWereForcedToWaitForAdditionalSupplies

      flavor do
        h "title"
        p.validate forcedToWait "readIntro1"
        p.validate (not forcedToWait) "readIntro2"

      flavor $ h "title" >> p (if forcedToWait then "intro1" else "intro2")
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure . TheDoomOfEztli $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup TheDoomOfEztli attrs $ setupTheDoomOfEztli attrs
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      push $ DrawAnotherChaosToken iid
      when (isHardExpert attrs) $ do
        withLocationOf iid \lid -> placeDoom ElderThing lid 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        ElderThing | isEasyStandard attrs -> do
          withLocationOf iid \lid -> placeDoom ElderThing lid 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      defeated <- select DefeatedInvestigator

      if null defeated
        then case r of
          NoResolution -> when (null defeated) (do_ R3)
          _ -> do_ msg
        else do
          yigsFury <- getRecordCount YigsFury
          storyOnlyBuild defeated $ scope "defeated" $ h "title" >> p "body"

          if yigsFury >= 4
            then do
              for_ defeated $ kill attrs
              investigators <- allInvestigators
              if length defeated == length investigators
                then gameOver
                else case r of
                  NoResolution -> do_ R2
                  _ -> do_ msg
            else do
              recordCount YigsFury (yigsFury + 3)
              case r of
                NoResolution -> do_ R3
                _ -> do_ msg

      pure s
    Do (ScenarioResolution n) -> scope "resolutions" do
      vengeance <- getTotalVengeanceInVictoryDisplay
      yigsFury <- getRecordCount YigsFury
      inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
      setAsideHarbinger <- selectOne $ OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.harbingerOfValusia
      let
        harbingerMessages = case inPlayHarbinger of
          Just harbinger -> recordCount TheHarbingerIsStillAlive =<< field EnemyDamage harbinger
          Nothing -> for_ setAsideHarbinger \harbinger -> do
            damage <-
              field @(OutOfPlayEntity 'SetAsideZone Enemy)
                (OutOfPlayEnemyField SetAsideZone EnemyDamage)
                harbinger
            recordCount TheHarbingerIsStillAlive damage

      case n of
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheInvestigatorsRecoveredTheRelicOfAges
          harbingerMessages
          recordCount YigsFury $ yigsFury + vengeance
          endOfScenario
          pure s
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          record AlejandroRecoveredTheRelicOfAges
          harbingerMessages
          recordCount YigsFury $ yigsFury + vengeance
          endOfScenario
          pure s
        Resolution 3 -> do
          resolution "resolution3"
          leadChooseOneM do
            labeled' "goBackInside" $ do_ R4
            labeled' "thisPlaceMustBeDestroyed" $ do_ R5
          pure s
        Resolution 4 -> do
          standalone <- getIsStandalone
          resolution "resolution4"
          pushAll
            $ ResetGame
            : [StandaloneSetup | standalone]
              <> [ ChooseLeadInvestigator
                 , SetupInvestigators
                 , SetChaosTokensForScenario -- (chaosBagOf campaign')
                 , InvestigatorsMulligan
                 , Setup
                 , EndSetup
                 ]
          let resetAttrs = toAttrs $ theDoomOfEztli attrs.difficulty
          let resolution4Count = toResultDefault @Int 0 attrs.meta + 1
          pure . TheDoomOfEztli $ resetAttrs & metaL .~ toJSON resolution4Count
        Resolution 5 -> do
          resolutionWithXp "resolution5" $ allGainXp' attrs
          record TheInvestigatorsRecoveredTheRelicOfAges
          harbingerMessages
          recordCount YigsFury (yigsFury + vengeance)
          endOfScenario
          pure s
        _ -> error "Unknown Resolution"
    ChooseLeadInvestigator -> do
      standalone <- getIsStandalone
      leader <- if standalone then pure Nothing else CampaignMeta.expeditionLeader <$> getCampaignMeta
      case leader of
        Just iid -> do
          push $ ChoosePlayer iid SetLeadInvestigator
          pure s
        Nothing -> TheDoomOfEztli <$> liftRunMessage msg attrs
    UseCardAbility _ ScenarioSource 1 _ _ -> do
      getEncounterDeck >>= \case
        Deck [] -> pure ()
        Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      pure s
    _ -> TheDoomOfEztli <$> liftRunMessage msg attrs
