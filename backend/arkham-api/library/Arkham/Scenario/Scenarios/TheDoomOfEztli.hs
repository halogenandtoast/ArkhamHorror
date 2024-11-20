module Arkham.Scenario.Scenarios.TheDoomOfEztli (TheDoomOfEztli (..), theDoomOfEztli) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Meta qualified as CampaignMeta
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (checkWhen, defeated)
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage)
import Arkham.Scenarios.TheDoomOfEztli.Story
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Arkham.Zone

newtype Metadata = Metadata {resolution4Count :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheDoomOfEztli = TheDoomOfEztli (ScenarioAttrs `With` Metadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The Doom of Eztli

For the layout Magic and Science will cause the layout to switch to using
the bottom line
-}
theDoomOfEztli :: Difficulty -> TheDoomOfEztli
theDoomOfEztli difficulty =
  scenario
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

instance HasChaosTokenValue TheDoomOfEztli where
  getChaosTokenValue iid chaosTokenFace (TheDoomOfEztli (attrs `With` _)) =
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
    { campaignLogRecorded = setFromList [TheInvestigatorsClearedAPathToTheEztliRuins]
    }

investigatorDefeat :: ReverseQueue m => ScenarioAttrs -> m ()
investigatorDefeat attrs = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    investigators <- allInvestigators
    yigsFury <- getRecordCount YigsFury
    if yigsFury >= 4
      then do
        story defeat
        story
          "The creatures are upon you before you have time to react. You scream in agony as you are skewered by razor-sharp spears."
        for_ defeated $ kill attrs
        when (length defeated == length investigators) gameOver
      else do
        story
          "Suddenly, a distant voice hisses to the others, and the serpents tentatively retreat into the darkness. You run for your life, not taking any chances."
        recordCount YigsFury (yigsFury + 3)

instance RunMessage TheDoomOfEztli where
  runMessage msg s@(TheDoomOfEztli (attrs `With` metadata)) = runQueueT $ case msg of
    PreScenarioSetup -> do
      forcedToWaitForAdditionalSupplies <-
        getHasRecord TheInvestigatorsWereForcedToWaitForAdditionalSupplies
      story $ if forcedToWaitForAdditionalSupplies then intro1 else intro2
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure . TheDoomOfEztli . (`with` metadata) $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup (TheDoomOfEztli . (`with` metadata)) attrs do
      gather Set.TheDoomOfEztli
      gather Set.AgentsOfYig
      gather Set.YigsVenom
      gather Set.TemporalFlux
      gather Set.DeadlyTraps
      gather Set.ForgottenRuins
      gather Set.Poison
      gather Set.ChillingCold

      entryway <- place Locations.entryway
      when (resolution4Count metadata > 0) do
        placeDoom attrs entryway (resolution4Count metadata)
      startAt entryway

      setAsidePoisonedCount <- getSetAsidePoisonedCount
      setAside
        $ [ Locations.chamberOfTime
          , Assets.relicOfAgesADeviceOfSomeSort
          , Enemies.harbingerOfValusia
          ]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      setActDeck [Acts.intoTheRuins, Acts.magicAndScience, Acts.escapeTheRuins]
      setAgendaDeck [Agendas.somethingStirs, Agendas.theTempleWarden]

      addExtraDeck ExplorationDeck
        =<< shuffle
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
        mlid <- field InvestigatorLocation iid
        for_ mlid $ \lid -> push $ PlaceTokens (ChaosTokenEffectSource ElderThing) (toTarget lid) Doom 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        ElderThing | isEasyStandard attrs -> do
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> push $ PlaceTokens (ChaosTokenEffectSource ElderThing) (toTarget lid) Doom 1
        _ -> pure ()
      pure s
    ScenarioResolution n -> do
      vengeance <- getVengeanceInVictoryDisplay
      lead <- getLead
      yigsFury <- getRecordCount YigsFury
      inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
      setAsideHarbinger <-
        selectOne
          $ OutOfPlayEnemy SetAsideZone
          $ enemyIs Enemies.harbingerOfValusia
      let
        harbingerMessages = case inPlayHarbinger of
          Just harbinger -> do
            damage <- field EnemyDamage harbinger
            recordCount TheHarbingerIsStillAlive damage
          Nothing -> case setAsideHarbinger of
            Nothing -> pure ()
            Just harbinger -> do
              damage <-
                field @(OutOfPlayEntity 'SetAsideZone Enemy)
                  (OutOfPlayEnemyField SetAsideZone EnemyDamage)
                  harbinger
              recordCount TheHarbingerIsStillAlive damage

      investigatorDefeat attrs
      let
        go = \case
          NoResolution -> do
            anyDefeated <- selectAny DefeatedInvestigator
            go $ Resolution $ if anyDefeated && yigsFury >= 4 then 2 else 3
          Resolution 1 -> do
            story resolution1
            record TheInvestigatorsRecoveredTheRelicOfAges
            harbingerMessages
            recordCount YigsFury (yigsFury + vengeance)
            allGainXp attrs
            endOfScenario
            pure s
          Resolution 2 -> do
            story resolution2
            record AlejandroRecoveredTheRelicOfAges
            harbingerMessages
            recordCount YigsFury (yigsFury + vengeance)
            allGainXp attrs
            endOfScenario
            pure s
          Resolution 3 -> do
            story resolution3
            chooseOne
              lead
              [ Label
                  "“We can’t stop now—we have to go back inside!” - Proceed to Resolution 4."
                  [ScenarioResolution $ Resolution 4]
              , Label
                  "“It’s too dangerous. This place must be destroyed.” - Proceed to Resolution 5."
                  [ScenarioResolution $ Resolution 5]
              ]
            pure s
          Resolution 4 -> do
            standalone <- getIsStandalone
            story resolution4
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
            pure . TheDoomOfEztli $ resetAttrs `with` Metadata (resolution4Count metadata + 1)
          Resolution 5 -> do
            story resolution5
            record TheInvestigatorsRecoveredTheRelicOfAges
            harbingerMessages
            recordCount YigsFury (yigsFury + vengeance)
            allGainXp attrs
            endOfScenario
            pure s
          _ -> error "Unknown Resolution"
      go n
    ChooseLeadInvestigator -> do
      standalone <- getIsStandalone
      leader <- if standalone then pure Nothing else CampaignMeta.expeditionLeader <$> getCampaignMeta
      case leader of
        Just iid -> do
          push $ ChoosePlayer iid SetLeadInvestigator
          pure s
        Nothing -> TheDoomOfEztli . (`with` metadata) <$> liftRunMessage msg attrs
    _ -> TheDoomOfEztli . (`with` metadata) <$> liftRunMessage msg attrs
