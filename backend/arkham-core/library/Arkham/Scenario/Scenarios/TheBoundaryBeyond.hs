module Arkham.Scenario.Scenarios.TheBoundaryBeyond
  ( TheBoundaryBeyond(..)
  , theBoundaryBeyond
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types ( Field (EnemyDamage) )
import Arkham.Helpers.Act
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (LocationCard, LocationName) )
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDamage )
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheBoundaryBeyond.Story
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype TheBoundaryBeyond = TheBoundaryBeyond ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The Boundary Beyond
-- For the location layout we make use of the location symbols rather than the
-- location names due to how replacement works.

theBoundaryBeyond :: Difficulty -> TheBoundaryBeyond
theBoundaryBeyond difficulty = scenario
  TheBoundaryBeyond
  "04161"
  "The Boundary Beyond"
  difficulty
  [ ".        .        .    circle  circle   .      .      ."
  , "triangle triangle star star    diamond diamond square square"
  , ".        .        .    heart   heart   .       .      ."
  ]

instance HasTokenValue TheBoundaryBeyond where
  getTokenValue iid tokenFace (TheBoundaryBeyond attrs) = case tokenFace of
    Skull -> do
      atAncientLocation <-
        selectAny
        $ LocationWithTrait Trait.Ancient
        <> locationWithInvestigator iid
      let n = if atAncientLocation then 2 else 0
      pure $ toTokenValue attrs Skull (1 + n) (2 + n)
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ toTokenValue attrs ElderThing 4 4
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

instance RunMessage TheBoundaryBeyond where
  runMessage msg s@(TheBoundaryBeyond attrs) = case msg of
    SetTokensForScenario -> do
      whenM getIsStandalone $ push $ SetTokens standaloneTokens
      pure s
    Setup -> do
      iids <- allInvestigatorIds
      forgedABondWithIchtaca <- getHasRecord
        TheInvestigatorsForgedABondWithIchtaca
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      withGasoline <- headMay <$> getInvestigatorsWithSupply Gasoline
      setAsidePoisonedCount <- getSetAsidePoisonedCount

      tokens <- getBagTokens
      let
        cultistCount = count ((== Cultist) . tokenFace) tokens
        tabletCount = count ((== Tablet) . tokenFace) tokens
        additionalSets =
          (guard (cultistCount >= 2)
            *> [EncounterSet.PnakoticBrotherhood, EncounterSet.DarkCult]
            )
            <> (guard (tabletCount >= 2)
               *> [EncounterSet.YigsVenom, EncounterSet.GuardiansOfTime]
               )
            <> (guard (cultistCount < 2 && tabletCount < 2)
               *> [ EncounterSet.PnakoticBrotherhood
                  , EncounterSet.GuardiansOfTime
                  ]
               )

      let
        explorationDeckLocations =
          [ Locations.temploMayor_174
          , Locations.temploMayor_175
          , Locations.templesOfTenochtitlan_176
          , Locations.templesOfTenochtitlan_177
          , Locations.chapultepecHill_178
          , Locations.chapultepecHill_179
          , Locations.canalsOfTenochtitlan_180
          , Locations.canalsOfTenochtitlan_181
          , Locations.lakeXochimilco_182
          , Locations.lakeXochimilco_183
          , Locations.sacredWoods_184
          , Locations.sacredWoods_185
          ]
        explorationDeckTreacheries =
          [ Treacheries.windowToAnotherTime
          , Treacheries.timelineDestabilization
          , Treacheries.aTearInTime
          , Treacheries.lostInTime
          ]

      encounterDeck' <-
        buildEncounterDeckExcluding
          (Enemies.padmaAmrita : explorationDeckLocations)
        $ [ EncounterSet.TheBoundaryBeyond
          , EncounterSet.TemporalFlux
          , EncounterSet.Poison
          ]
        <> additionalSets

      let
        encounterDeck =
          removeEachFromDeck encounterDeck' explorationDeckTreacheries


      metropolitanCathedral <- genCard Locations.metropolitanCathedral
      zocalo <- genCard Locations.zocalo
      templeRuins <- genCard Locations.templeRuins
      xochimilco <- genCard Locations.xochimilco
      chapultepecPark <- genCard Locations.chapultepecPark
      coyoacan <- genCard Locations.coyoacan

      explorationDeck <- shuffleM =<< traverse
        genCard
        (explorationDeckLocations <> explorationDeckTreacheries)

      setAsideCards <-
        traverse genCard
        $ [Enemies.padmaAmrita, Acts.theReturnTrip, Agendas.timeCollapsing]
        <> replicate setAsidePoisonedCount Treacheries.poisoned

      isStandalone <- getIsStandalone

      pushAll
        $ [story iids introPart1]
        <> [ story iids
               $ if forgedABondWithIchtaca then ichtacasQuest else silentJourney
           | not isStandalone
           ]
        <> [ story iids $ if foundTheMissingRelic
               then arcaneThrumming
               else growingConcern
           | not isStandalone
           ]
        <> [ story iids
               $ if rescuedAlejandro then alejandrosThoughts else anEmptySeat
           | not isStandalone
           ]
        <> [ story iids outOfGas | not isStandalone && isNothing withGasoline ]
        <> [ UseSupply iid Gasoline
           | not isStandalone
           , iid <- maybeToList withGasoline
           ]
        <> [story iids introPart2]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , PlaceLocation metropolitanCathedral
           , PlaceLocation zocalo
           , PlaceLocation templeRuins
           , PlaceLocation xochimilco
           , PlaceLocation chapultepecPark
           , PlaceLocation coyoacan
           ]
        <> [ chooseOne
               iid
               [ targetLabel lid [MoveTo (toSource attrs) iid lid]
               | l <- [zocalo, coyoacan]
               , let lid = toLocationId l
               ]
           | iid <- iids
           ]
      TheBoundaryBeyond <$> runMessage
        msg
        (attrs
        & (decksL . at ExplorationDeck ?~ explorationDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [Acts.crossingTheThreshold, Acts.pastAndPresent]
          )
        & (agendaStackL
          . at 1
          ?~ [Agendas.theBoundaryBroken, Agendas.theBarrierIsThin]
          )
        )
    Explore iid _ _ -> do
      windowMsg <- checkWindows [Window Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher ReplaceExplored 1
      pure s
    AddToVictory (LocationTarget lid) -> do
      -- We want to replace the card ID to avoid UI confusion
      -- TODO: We REALLY want card ids and instance ids to be different
      card <- field LocationCard lid
      newId <- getRandom
      let
        card' = case card of
          EncounterCard ec -> EncounterCard $ ec { ecId = newId }
          _ -> error "Unhandled"
      pure . TheBoundaryBeyond $ attrs & (victoryDisplayL %~ (card' :))
    RemoveLocation lid -> do
      -- we handle remove location special because we need to replace it
      title <- fieldMap LocationName nameTitle lid
      let
        replacement = case title of
          "Templo Mayor" -> Locations.templeRuins
          "Temples of Tenochtitlán" -> Locations.metropolitanCathedral
          "Chapultepec Hill" -> Locations.chapultepecPark
          "Canals of Tenochtitlán" -> Locations.zocalo
          "Lake Xochimilco" -> Locations.xochimilco
          "Sacred Woods" -> Locations.coyoacan
          _ -> error $ "Unmatched location title: " <> show title
      card <- genCard replacement
      push $ ReplaceLocation lid card
      pure s
    ResolveToken _ tokenFace iid | tokenFace `elem` [Cultist, Tablet] -> do
      push $ DrawAnotherToken iid
      pure s
    ResolveToken _ ElderThing iid | isHardExpert attrs -> do
      targets <-
        selectListMap LocationTarget $ NearestLocationToYou $ LocationWithTrait
          Trait.Ancient
      unless (null targets) $ push $ chooseOrRunOne
        iid
        [ TargetLabel target [PlaceClues target 1] | target <- targets ]
      pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        Cultist -> do
          targets <- selectListMap EnemyTarget $ EnemyWithTrait Trait.Cultist
          when (notNull targets) $ if isEasyStandard attrs
            then push $ chooseOne
              iid
              [ TargetLabel target [PlaceDoom target 1] | target <- targets ]
            else pushAll $ map (`PlaceDoom` 1) targets
        Tablet -> do
          serpents <- selectList $ EnemyWithTrait Trait.Serpent <> EnemyAt
            (locationWithInvestigator iid)
          when (notNull serpents) $ if isEasyStandard attrs
            then push $ chooseOne
              iid
              [ targetLabel
                  serpent
                  [EnemyAttack iid serpent DamageAny RegularAttack]
              | serpent <- serpents
              ]
            else pushAll
              [ EnemyAttack iid serpent DamageAny RegularAttack
              | serpent <- serpents
              ]
        ElderThing | isEasyStandard attrs -> do
          targets <-
            selectListMap LocationTarget
            $ NearestLocationToYou
            $ LocationWithTrait Trait.Ancient
          unless (null targets) $ push $ chooseOrRunOne
            iid
            [ TargetLabel target [PlaceClues target 1] | target <- targets ]
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      iids <- allInvestigatorIds
      vengeance <- getVengeanceInVictoryDisplay
      yigsFury <- getRecordCount YigsFury
      inVictory <- selectAny $ VictoryDisplayCardMatch $ cardIs
        Enemies.harbingerOfValusia
      inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
      damage <- case inPlayHarbinger of
        Just eid -> field EnemyDamage eid
        Nothing -> getRecordCount TheHarbingerIsStillAlive
      step <- getCurrentActStep
      locations <-
        selectListMap LocationTarget
        $ LocationWithTrait Trait.Tenochtitlan
        <> LocationWithoutClues

      let
        storyPassage = case resolution of
          NoResolution -> noResolution
          Resolution 1 -> resolution1
          Resolution 2 -> resolution2
          _ -> error "invalid resolution"
        addLocationsToVictory = and
          [ step == 2
          , notNull locations
          , resolution `elem` [NoResolution, Resolution 2]
          ]

      pushAll
        $ [story iids storyPassage]
        <> (if addLocationsToVictory then map AddToVictory locations else [])
        <> [ScenarioResolutionStep 1 resolution]
        <> [RecordCount YigsFury (yigsFury + vengeance)]
        <> [ CrossOutRecord TheHarbingerIsStillAlive | inVictory ]
        <> [ RecordCount TheHarbingerIsStillAlive damage | not inVictory ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolutionStep 1 resolution -> do
      n <- selectCount $ VictoryDisplayCardMatch $ CardWithTrait
        Trait.Tenochtitlan
      gainXp <- map (uncurry GainXP) <$> getXpWithBonus n
      pushAll
        $ RecordCount PathsAreKnownToYou n
        : [ Record IchtacaHasConfidenceInYou
          | n >= 3 && resolution == Resolution 1
          ]
        <> gainXp
      pure s
    _ -> TheBoundaryBeyond <$> runMessage msg attrs
