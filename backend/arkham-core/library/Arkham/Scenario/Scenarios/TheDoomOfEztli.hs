module Arkham.Scenario.Scenarios.TheDoomOfEztli
  ( TheDoomOfEztli(..)
  , theDoomOfEztli
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message hiding ( EnemyDamage )
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheDoomOfEztli.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Metadata = Metadata { resolution4Count :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheDoomOfEztli = TheDoomOfEztli (ScenarioAttrs `With` Metadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The Doom of Eztli
--
-- For the layout Magic and Science will cause the layout to switch to using
-- the bottom line
theDoomOfEztli :: Difficulty -> TheDoomOfEztli
theDoomOfEztli difficulty = scenario
  (TheDoomOfEztli . (`with` Metadata 0))
  "04054"
  "The Doom of Eztli"
  difficulty
  [ ".        ancientHall  undergroundRuins .             .             .    ."
  , "entryway ancientHall  undergroundRuins secretPassage chamberOfTime .    ."
  , "entryway grandChamber burialPit        secretPassage chamberOfTime .    ."
  , ".        grandChamber burialPit        .             .             .    ."
  , "pos1     pos2         pos3             pos4          pos5          pos6 pos7"
  ]

instance HasTokenValue TheDoomOfEztli where
  getTokenValue iid tokenFace (TheDoomOfEztli (attrs `With` _)) =
    case tokenFace of
      Skull -> do
        hasDoom <-
          selectAny $ LocationWithAnyDoom <> locationWithInvestigator iid
        pure $ if hasDoom
          then toTokenValue attrs Skull 3 4
          else toTokenValue attrs Skull 1 2
      face | face `elem` [Cultist, Tablet] -> do
        n <- if isEasyStandard attrs
          then selectCount LocationWithAnyDoom
          else getSum <$> selectAgg Sum LocationDoom LocationWithAnyDoom
        pure $ TokenValue Cultist (NegativeModifier n)
      ElderThing -> pure $ TokenValue ElderThing NoModifier
      otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
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
standaloneCampaignLog = mkCampaignLog
  { campaignLogRecorded = setFromList
    [TheInvestigatorsClearedAPathToTheEztliRuins]
  }

investigatorDefeat :: HasGame m => ScenarioAttrs -> m [Message]
investigatorDefeat attrs = do
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
  if null defeatedInvestigatorIds
    then pure []
    else do
      investigatorIds <- allInvestigatorIds
      yigsFury <- getRecordCount YigsFury
      if yigsFury >= 4
        then do
          if null defeatedInvestigatorIds
            then pure []
            else
              pure
              $ story investigatorIds defeat
              : story
                  investigatorIds
                  "The creatures are upon you before you have time to react. You scream in agony as you are skewered by razor-sharp spears."
              : map
                  (InvestigatorKilled (toSource attrs))
                  defeatedInvestigatorIds
              <> [ GameOver
                 | null
                   (setFromList @(HashSet InvestigatorId) investigatorIds
                   `difference` setFromList @(HashSet InvestigatorId)
                                  defeatedInvestigatorIds
                   )
                 ]
        else do
          pure
            [ story
              investigatorIds
              "Suddenly, a distant voice hisses to the others, and the serpents tentatively retreat into the darkness. You run for your life, not taking any chances."
            , RecordCount YigsFury (yigsFury + 3)
            ]

instance RunMessage TheDoomOfEztli where
  runMessage msg s@(TheDoomOfEztli (attrs `With` metadata)) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    StandaloneSetup ->
      pure
        . TheDoomOfEztli
        . (`with` metadata)
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      iids <- allInvestigatorIds
      -- | Determine intro
      forcedToWaitForAdditionalSupplies <- getHasRecord
        TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      let intro = if forcedToWaitForAdditionalSupplies then intro1 else intro2
      -- | Setup
      -- -- | Gather cards
      encounterDeck <- buildEncounterDeckExcluding
        [ Enemies.harbingerOfValusia
        , Locations.ancientHall
        , Locations.grandChamber
        , Locations.undergroundRuins
        , Locations.burialPit
        , Locations.secretPassage
        , Locations.chamberOfTime
        ]
        [ EncounterSet.TheDoomOfEztli
        , EncounterSet.AgentsOfYig
        , EncounterSet.YigsVenom
        , EncounterSet.TemporalFlux
        , EncounterSet.DeadlyTraps
        , EncounterSet.ForgottenRuins
        , EncounterSet.Poison
        , EncounterSet.ChillingCold
        ]

      let
        encounterDeck' = removeEachFromDeck
          encounterDeck
          [ Treacheries.illOmen
          , Treacheries.deepDark
          , Treacheries.finalMistake
          , Treacheries.entombed
          , Treacheries.cryptChill
          ]

      -- Put entryway into play investigators start there
      entryway <- genCard Locations.entryway
      -- | Messages

      explorationDeck <- shuffleM =<< traverse
        genCard
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

      setAsidePoisonedCount <- getSetAsidePoisonedCount

      setAsideCards <-
        traverse genCard
        $ [ Locations.chamberOfTime
          , Assets.relicOfAgesADeviceOfSomeSort
          , Enemies.harbingerOfValusia
          ]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      pushAll
        $ [ story iids intro
          , SetEncounterDeck encounterDeck'
          , SetAgendaDeck
          , SetActDeck
          , PlaceLocation entryway
          , RevealLocation Nothing (toLocationId entryway)
          ]
        <> [ PlaceDoom
               (LocationTarget $ toLocationId entryway)
               (resolution4Count metadata)
           | resolution4Count metadata > 0
           ]
        <> [MoveAllTo (toSource attrs) (toLocationId entryway)]

      TheDoomOfEztli . (`with` metadata) <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (agendaStackL
          . at 1
          ?~ [Agendas.somethingStirs, Agendas.theTempleWarden]
          )
        & (actStackL
          . at 1
          ?~ [Acts.intoTheRuins, Acts.magicAndScience, Acts.escapeTheRuins]
          )
        )
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher PlaceExplored 1
      pure s
    ResolveToken _ ElderThing iid -> do
      push $ DrawAnotherToken iid
      when (isHardExpert attrs) $ do
        mlid <- field InvestigatorLocation iid
        for_ mlid $ \lid -> push $ PlaceDoom (LocationTarget lid) 1
      pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        ElderThing | isEasyStandard attrs -> do
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> push $ PlaceDoom (LocationTarget lid) 1
        _ -> pure ()
      pure s
    ScenarioResolution n -> do
      vengeance <- getVengeanceInVictoryDisplay
      investigatorIds <- allInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      yigsFury <- getRecordCount YigsFury
      defeatMessages <- investigatorDefeat attrs
      gainXp <- map (uncurry GainXP) <$> getXp
      inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
      setAsideHarbinger <- selectOne $ SetAsideMatcher $ enemyIs
        Enemies.harbingerOfValusia
      harbingerMessages <- case inPlayHarbinger of
        Just harbinger -> do
          damage <- field EnemyDamage harbinger
          pure [RecordCount TheHarbingerIsStillAlive damage]
        Nothing -> case setAsideHarbinger of
          Nothing -> pure []
          Just harbinger -> do
            damage <- field (SetAsideEnemyField EnemyDamage) harbinger
            pure [RecordCount TheHarbingerIsStillAlive damage]

      case n of
        NoResolution -> do
          anyDefeated <- selectAny DefeatedInvestigator
          let resolution = if anyDefeated && yigsFury >= 4 then 2 else 3
          push $ ScenarioResolution (Resolution resolution)
          pure s
        Resolution 1 -> do
          pushAll
            $ defeatMessages
            <> [ story investigatorIds resolution1
               , Record TheInvestigatorsRecoveredTheRelicOfAges
               ]
            <> harbingerMessages
            <> [RecordCount YigsFury (yigsFury + vengeance)]
            <> gainXp
            <> [EndOfGame Nothing]
          pure s
        Resolution 2 -> do
          pushAll
            $ defeatMessages
            <> [ story investigatorIds resolution2
               , Record AlejandroRecoveredTheRelicOfAges
               ]
            <> harbingerMessages
            <> [RecordCount YigsFury (yigsFury + vengeance)]
            <> gainXp
            <> [EndOfGame Nothing]
          pure s
        Resolution 3 -> do
          pushAll
            $ defeatMessages
            <> [ story investigatorIds resolution3
               , chooseOne
                 leadInvestigatorId
                 [ Label
                   "“We can’t stop now—we have to go back inside!” - Proceed to Resolution 4."
                   [ScenarioResolution $ Resolution 4]
                 , Label
                   "“It’s too dangerous. This place must be destroyed.” - Proceed to Resolution 5."
                   [ScenarioResolution $ Resolution 5]
                 ]
               ]
          pure s
        Resolution 4 -> do
          standalone <- getIsStandalone
          pushAll
            $ [story investigatorIds resolution4, ResetGame]
            <> [ StandaloneSetup | standalone ]
            <> [ ChooseLeadInvestigator
               , SetupInvestigators
               , SetTokensForScenario -- (chaosBagOf campaign')
               , InvestigatorsMulligan
               , Setup
               , EndSetup
               ]
          let resetAttrs = toAttrs $ theDoomOfEztli (scenarioDifficulty attrs)
          pure . TheDoomOfEztli $ resetAttrs `with` Metadata
            (resolution4Count metadata + 1)
        Resolution 5 -> do
          pushAll
            $ [ story investigatorIds resolution5
              , Record TheInvestigatorsRecoveredTheRelicOfAges
              ]
            <> harbingerMessages
            <> [RecordCount YigsFury (yigsFury + vengeance)]
            <> gainXp
            <> [EndOfGame Nothing]
          pure s
        _ -> error "Unknown Resolution"
    _ -> TheDoomOfEztli . (`with` metadata) <$> runMessage msg attrs
