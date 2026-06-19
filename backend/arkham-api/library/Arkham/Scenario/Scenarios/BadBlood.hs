module Arkham.Scenario.Scenarios.BadBlood (badBlood) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Exception
import {-# SOURCE #-} Arkham.GameEnv (findCard)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..), Location, locationPlacedChaosTokens)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.BadBlood.Helpers
import Arkham.Scenarios.BadBlood.Meta
import Arkham.Token (Token (Memory), countTokens)
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window qualified as Window
import Arkham.Xp

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , MinusFive
  , MinusSix , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix , MinusSeven
  , MinusEight , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

newtype BadBlood = BadBlood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

badBlood :: Difficulty -> BadBlood
badBlood difficulty =
  sideStory
    (BadBlood . (metaL .~ toJSON emptyMeta))
    "90020"
    "Bad Blood"
    difficulty
    [ ".                .                trainTracks  trainTracks          townHall             townHall  arkhamPoliceStation  arkhamPoliceStation .           ."
    , "curiositieShoppe curiositieShoppe northside    northside            downtown             downtown  easttown             easttown            velmasDiner velmasDiner"
    , ".                eztliExhibit     eztliExhibit miskatonicUniversity miskatonicUniversity rivertown rivertown            blackCave           blackCave   ."
    ]

instance HasChaosTokenValue BadBlood where
  getChaosTokenValue iid tokenFace (BadBlood attrs) = case tokenFace of
    Skull -> do
      n <- (.agnesMemories) <$> getBadBloodMeta
      pure $ ChaosTokenValue Skull $ NegativeModifier $ if isEasyStandard attrs then n else n + 1
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 6 8
    otherFace -> getChaosTokenValue iid otherFace attrs

tokenMemoryValue :: (HasGame m, Tracing m) => InvestigatorId -> ChaosTokenFace -> m Int
tokenMemoryValue lead = \case
  AutoFail -> pure 6
  ElderSign -> pure 6
  face -> do
    value <- getChaosTokenValue lead face ()
    maybe 0 abs <$> chaosTokenValue value

gainXpWithMemories :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m Int
gainXpWithMemories attrs = do
  n <- (.agnesMemories) <$> getBadBloodMeta
  magnes <- selectOne $ IncludeEliminated agnesBaker
  (initial, details) <- getXp'
  let adjust (iid, x) = (iid, if Just iid == magnes then max x n else x)
  let
    bonusEntries = do
      agnes <- maybeToList magnes
      x <- maybeToList $ lookup agnes details
      guard $ n > x
      pure
        $ InvestigatorGainXp agnes
        $ XpDetail XpFromCardEffect (scope "xp" $ "$" <> ikey "memories") (n - x)
  push . ReportXp . (<> XpBreakdown bonusEntries) =<< generateXpReport NoBonus
  pushAll =<< toGainXp attrs (pure $ map adjust details)
  pure initial

instance RunMessage BadBlood where
  runMessage msg s@(BadBlood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "part1"
      flavor $ h "title" >> p "part2"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup BadBlood attrs do
      setup $ ul do
        li "gatherSets"
        li "scenarioReference"
        li.nested "placeLocations" do
          li "startAt"
        li "randomLocation"
        li "actAgendaDecks"
        li "removeStoryAssets"
        li "placeElspeth"
        li "placeMemories"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.BadBlood
      gather Set.ThreadsOfFate
      gather Set.PnakoticBrotherhood
      gather Set.LockedDoors
      gather Set.Nightgaunts
      gather Set.DarkCult
      gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow, Treacheries.falseLead]

      removeEvery [Assets.ichtacaTheForgottenGuardian, Assets.expeditionJournal]

      setAgendaDeck [Agendas.hyperboreanBlood]
      setActDeck [Acts.aWalkDownMemoryLane]

      velmasDiner <- place Locations.velmasDiner
      startAt velmasDiner

      fixedLocations <-
        placeAllCapture
          [ Locations.northside
          , Locations.downtownFirstBankOfArkham
          , Locations.easttown
          , Locations.miskatonicUniversity
          , Locations.rivertown
          , Locations.townHall
          ]

      curiositieShoppe <- place Locations.curiositieShoppe

      randomLocation <-
        placeOneOf
          $ Locations.eztliExhibit
          :| [Locations.blackCave, Locations.trainTracks, Locations.arkhamPoliceStation]

      enemyAt_ Enemies.elspethBaudin curiositieShoppe

      for_ ([velmasDiner, curiositieShoppe, randomLocation] <> fixedLocations) \lid ->
        placeTokens attrs lid Memory 1
    ScenarioSpecific "agnesCollectsMemory" v -> do
      let lid = toResult @LocationId v
      removeTokens ScenarioSource lid Memory 1
      meta <- getBadBloodMeta
      let n = meta.agnesMemories + 1
      send $ countVar n $ ikey' "log.agnesCollectsMemory"
      -- checkMemoryTokens
      checkAfter $ Window.ScenarioEvent "memoryCollected" Nothing Null
      pure . BadBlood $ attrs & metaL .~ toJSON meta {agnesMemories = n}
    ScenarioSpecific "elspethCollectsMemory" v -> do
      let lid = toResult @LocationId v
      removeTokens ScenarioSource lid Memory 1
      whenJustM (selectOne $ enemyIs Enemies.elspethBaudin) \eid ->
        placeTokens ScenarioSource eid Memory 1
      meta <- getBadBloodMeta
      let n = meta.elspethMemories + 1
      send $ countVar n $ ikey' "log.elspethCollectsMemory"
      -- checkMemoryTokens
      checkAfter $ Window.ScenarioEvent "memoryCollected" Nothing Null
      pure . BadBlood $ attrs & metaL .~ toJSON meta {elspethMemories = n}
    ScenarioSpecific "agnesStealsMemory" _ -> do
      meta <- getBadBloodMeta
      send $ ikey' "log.agnesStealsMemory"
      whenJustM (selectOne $ enemyIs Enemies.elspethBaudin) \eid ->
        removeTokens ScenarioSource eid Memory 1
      checkAfter $ Window.ScenarioEvent "memoryCollected" Nothing Null
      pure
        . BadBlood
        $ attrs
        & metaL
        .~ toJSON
          meta {agnesMemories = meta.agnesMemories + 1, elspethMemories = meta.elspethMemories - 1}
    ScenarioSpecific "checkMemoryTokens" _ -> do
      lead <- getLead
      locations <- select Anywhere
      for_ locations \lid -> do
        tokens <- locationPlacedChaosTokens <$> getAttrs @Location lid
        unless (null tokens) do
          hasMemory <- fieldMap LocationTokens ((> 0) . countTokens Memory) lid
          if not hasMemory
            then for_ tokens removePlacedChaosToken
            else do
              total <- sum <$> traverse (tokenMemoryValue lead . (.face)) tokens
              when (total >= 6) do
                for_ tokens removePlacedChaosToken
                elspethCollectsMemoryAt lid
      pure s
    ResolveChaosToken _ Cultist _ -> do
      selectOne (enemyIs Enemies.elspethBaudin) >>= traverse_ \elspeth -> do
        readyThis elspeth
        whenM (elspeth <=~> UnengagedEnemy) $ push $ PatrolMove elspeth memoryLocation
        when (isHardExpert attrs) do
          selectEach (investigatorEngagedWith elspeth) \engaged ->
            initiateEnemyAttackEdit elspeth Cultist engaged despiteExhausted
      pure s
    ResolveChaosToken _ ElderThing _ -> do
      whenJustM (selectOne agnesBaker) \agnes ->
        chooseAmount' agnes "elderThingDamage" "$damage" 0 3 attrs
      pure s
    ResolveAmounts _ (getChoiceAmount "$damage" -> n) (isTarget attrs -> True) | n > 0 -> do
      whenJustM (selectOne agnesBaker) \agnes -> assignDamage agnes ElderThing n
      withSkillTest \sid ->
        getSkillTestInvestigator >>= traverse_ \iid ->
          skillTestModifier sid ElderThing iid (AnySkillValue (2 * n))
      pure s
    FailedSkillTest _iid _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      selectOne (enemyIs Enemies.elspethBaudin <> EnemyAt memoryLocation) >>= traverse_ \elspeth ->
        selectOne (locationWithEnemy elspeth) >>= traverse_ \lid -> do
          placeChaosToken lid token
      -- checkMemoryTokens
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ gainXpWithMemories attrs
          whenJustM (selectOne $ IncludeEliminated agnesBaker) \agnes -> do
            hasHeirloom <- isJust <$> findCard (`cardMatch` cardIs Assets.heirloomOfHyperborea)
            hasAdvancedDarkMemory <- isJust <$> findCard (`cardMatch` cardIs Events.darkMemoryAdvanced)
            when (hasHeirloom || hasAdvancedDarkMemory) do
              chooseOneM agnes do
                questionLabeled' "chooseSwap"
                when hasHeirloom $ labeled' "upgradeHeirloomOfHyperborea" do
                  removeCampaignCardFromDeck agnes Assets.heirloomOfHyperborea
                  addCampaignCardToDeck agnes DoNotShuffleIn Assets.heirloomOfHyperboreaAdvanced
                when hasAdvancedDarkMemory $ labeled' "downgradeDarkMemory" do
                  removeCampaignCardFromDeck agnes Events.darkMemoryAdvanced
                  addCampaignCardToDeck agnes DoNotShuffleIn Events.darkMemory
                labeled' "doNotSwap" nothing
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ gainXpWithMemories attrs
          whenJustM (selectOne $ IncludeEliminated agnesBaker) \agnes -> do
            hasDarkMemory <- isJust <$> findCard (`cardMatch` cardIs Events.darkMemory)
            hasAdvancedHeirloom <-
              isJust <$> findCard (`cardMatch` cardIs Assets.heirloomOfHyperboreaAdvanced)
            when (hasDarkMemory || hasAdvancedHeirloom) do
              chooseOrRunOneM agnes do
                questionLabeled' "mustSwap"
                when hasDarkMemory $ labeled' "upgradeDarkMemory" do
                  removeCampaignCardFromDeck agnes Events.darkMemory
                  addCampaignCardToDeck agnes DoNotShuffleIn Events.darkMemoryAdvanced
                when hasAdvancedHeirloom $ labeled' "downgradeHeirloomOfHyperborea" do
                  removeCampaignCardFromDeck agnes Assets.heirloomOfHyperboreaAdvanced
                  addCampaignCardToDeck agnes DoNotShuffleIn Assets.heirloomOfHyperborea
          endOfScenario
        _ -> throw $ UnknownResolution r
      pure s
    _ -> BadBlood <$> liftRunMessage msg attrs
