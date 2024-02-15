module Arkham.Scenario.Scenarios.TheBoundaryBeyond (
  TheBoundaryBeyond (..),
  theBoundaryBeyond,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Deck
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationName))
import Arkham.Matcher
import Arkham.Movement
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheBoundaryBeyond.Story
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype TheBoundaryBeyond = TheBoundaryBeyond ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The Boundary Beyond
For the location layout we make use of the location symbols rather than the
location names due to how replacement works.
-}
theBoundaryBeyond :: Difficulty -> TheBoundaryBeyond
theBoundaryBeyond difficulty =
  scenario
    TheBoundaryBeyond
    "04161"
    "The Boundary Beyond"
    difficulty
    [ ".        .        .    circle  circle   .      .      ."
    , "triangle triangle star star    diamond diamond square square"
    , ".        .        .    heart   heart   .       .      ."
    ]

instance HasChaosTokenValue TheBoundaryBeyond where
  getChaosTokenValue iid chaosTokenFace (TheBoundaryBeyond attrs) = case chaosTokenFace of
    Skull -> do
      atAncientLocation <-
        selectAny
          $ LocationWithTrait Trait.Ancient
          <> locationWithInvestigator iid
      let n = if atAncientLocation then 2 else 0
      pure $ toChaosTokenValue attrs Skull (1 + n) (2 + n)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 4
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

instance RunMessage TheBoundaryBeyond where
  runMessage msg s@(TheBoundaryBeyond attrs) = case msg of
    PreScenarioSetup -> do
      iids <- allInvestigatorIds
      players <- allPlayers
      forgedABondWithIchtaca <-
        getHasRecord
          TheInvestigatorsForgedABondWithIchtaca
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      withGasoline <- headMay <$> getInvestigatorsWithSupply Gasoline
      mRelicOwner <- getOwner Assets.relicOfAgesADeviceOfSomeSort
      isStandalone <- getIsStandalone

      if isStandalone
        then pushAll [story players introPart1, story players introPart2]
        else
          pushAll
            $ [story players introPart1]
            <> ( if forgedABondWithIchtaca
                  then [story players ichtacasQuest]
                  else
                    story players silentJourney
                      : [ CreateWindowModifierEffect
                          EffectSetupWindow
                          ( EffectModifiers
                              $ toModifiers attrs [StartingHand (-2)]
                          )
                          (toSource attrs)
                          (InvestigatorTarget iid)
                        | iid <- iids
                        ]
               )
            <> ( if foundTheMissingRelic
                  then
                    story players arcaneThrumming
                      : RemoveCampaignCard Assets.relicOfAgesADeviceOfSomeSort
                      : [ AddCampaignCardToDeck
                          ownerId
                          Assets.relicOfAgesForestallingTheFuture
                        | ownerId <- maybeToList mRelicOwner
                        ]
                  else [story players growingConcern]
               )
            <> ( if rescuedAlejandro
                  then
                    story players alejandrosThoughts
                      : [ CreateWindowModifierEffect
                          EffectSetupWindow
                          ( EffectModifiers
                              $ toModifiers attrs [StartingResources 2]
                          )
                          (toSource attrs)
                          (InvestigatorTarget iid)
                        | iid <- iids
                        ]
                  else [story players anEmptySeat]
               )
            <> ( if isNothing withGasoline
                  then
                    story players outOfGas
                      : [ CreateWindowModifierEffect
                          EffectSetupWindow
                          (EffectModifiers $ toModifiers attrs [CannotMulligan])
                          (toSource attrs)
                          (InvestigatorTarget iid)
                        | iid <- iids
                        ]
                  else []
               )
            <> [UseSupply iid Gasoline | iid <- maybeToList withGasoline]
            <> [story players introPart2]
      pure s
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ push $ SetChaosTokens standaloneChaosTokens
      pure s
    Setup -> do
      setAsidePoisonedCount <- getSetAsidePoisonedCount

      tokens <- getBagChaosTokens
      let
        cultistCount = count ((== Cultist) . chaosTokenFace) tokens
        tabletCount = count ((== Tablet) . chaosTokenFace) tokens
        additionalSets =
          ( guard (cultistCount >= 2)
              *> [EncounterSet.PnakoticBrotherhood, EncounterSet.DarkCult]
          )
            <> ( guard (tabletCount >= 2)
                  *> [EncounterSet.YigsVenom, EncounterSet.GuardiansOfTime]
               )
            <> ( guard (cultistCount < 2 && tabletCount < 2)
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

      placeMetropolitanCathedral <-
        placeLocationCard_
          Locations.metropolitanCathedral
      (zocaloId, placeZocalo) <- placeLocationCard Locations.zocalo
      placeTempleRuins <- placeLocationCard_ Locations.templeRuins
      placeXochimilco <- placeLocationCard_ Locations.xochimilco
      placeChapultepecPark <- placeLocationCard_ Locations.chapultepecPark
      (coyoacanId, placeCoyoacan) <- placeLocationCard Locations.coyoacan

      explorationDeck <-
        shuffleM
          =<< genCards (explorationDeckLocations <> explorationDeckTreacheries)

      setAsideCards <-
        genCards
          $ [Enemies.padmaAmrita, Acts.theReturnTrip, Agendas.timeCollapsing]
          <> replicate setAsidePoisonedCount Treacheries.poisoned

      investigatorPlayers <- allInvestigatorPlayers

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeMetropolitanCathedral
          , placeZocalo
          , placeTempleRuins
          , placeXochimilco
          , placeChapultepecPark
          , placeCoyoacan
          ]
        <> [ chooseOne
            player
            [ targetLabel lid [MoveTo $ move attrs iid lid]
            | lid <- [zocaloId, coyoacanId]
            ]
           | (iid, player) <- investigatorPlayers
           ]
      agendas <- genCards [Agendas.theBoundaryBroken, Agendas.theBarrierIsThin]
      acts <- genCards [Acts.crossingTheThreshold, Acts.pastAndPresent]
      TheBoundaryBeyond
        <$> runMessage
          msg
          ( attrs
              & (decksL . at ExplorationDeck ?~ explorationDeck)
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    Explore iid _ _ -> do
      windowMsg <- checkWindows [mkWindow Timing.When $ Window.AttemptExplore iid]
      pushAll [windowMsg, Do msg]
      pure s
    Do (Explore iid source locationMatcher) -> do
      explore iid source locationMatcher ReplaceExplored 1
      pure s
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
      push $ ReplaceLocation lid card DefaultReplace
      pure s
    ResolveChaosToken _ chaosTokenFace iid | chaosTokenFace `elem` [Cultist, Tablet] -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      targets <-
        selectMap LocationTarget
          $ NearestLocationToYou
          $ LocationWithTrait
            Trait.Ancient
      player <- getPlayer iid
      unless (null targets)
        $ push
        $ chooseOrRunOne
          player
          [targetLabel target [PlaceTokens (toSource attrs) target Clue 1] | target <- targets]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      player <- getPlayer iid
      case chaosTokenFace token of
        Cultist -> do
          targets <- selectMap EnemyTarget $ EnemyWithTrait Trait.Cultist
          when (notNull targets)
            $ if isEasyStandard attrs
              then
                push
                  $ chooseOne
                    player
                    [targetLabel target [PlaceTokens (toSource attrs) target Doom 1] | target <- targets]
              else pushAll $ map (\t -> PlaceTokens (toSource attrs) t Doom 1) targets
        Tablet -> do
          serpents <-
            select
              $ EnemyWithTrait Trait.Serpent
              <> EnemyAt
                (locationWithInvestigator iid)
          when (notNull serpents)
            $ if isEasyStandard attrs
              then
                push
                  $ chooseOne
                    player
                    [ targetLabel serpent [EnemyAttack $ enemyAttack serpent attrs iid]
                    | serpent <- serpents
                    ]
              else
                pushAll
                  [EnemyAttack $ enemyAttack serpent attrs iid | serpent <- serpents]
        ElderThing | isEasyStandard attrs -> do
          targets <-
            selectMap LocationTarget
              $ NearestLocationToYou
              $ LocationWithTrait Trait.Ancient
          unless (null targets)
            $ push
            $ chooseOrRunOne
              player
              [ targetLabel target [PlaceTokens (ChaosTokenEffectSource ElderThing) target Clue 1]
              | target <- targets
              ]
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      players <- allPlayers
      vengeance <- getVengeanceInVictoryDisplay
      yigsFury <- getRecordCount YigsFury
      inVictory <-
        selectAny
          $ VictoryDisplayCardMatch
          $ cardIs
            Enemies.harbingerOfValusia
      inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
      damage <- case inPlayHarbinger of
        Just eid -> field EnemyDamage eid
        Nothing -> getRecordCount TheHarbingerIsStillAlive
      step <- getCurrentActStep
      locations <-
        selectMap LocationTarget
          $ LocationWithTrait Trait.Tenochtitlan
          <> LocationWithoutClues

      let
        storyPassage = case resolution of
          NoResolution -> noResolution
          Resolution 1 -> resolution1
          Resolution 2 -> resolution2
          _ -> error "invalid resolution"
        addLocationsToVictory =
          and
            [ step == 2
            , notNull locations
            , resolution `elem` [NoResolution, Resolution 2]
            ]

      pushAll
        $ [story players storyPassage]
        <> (if addLocationsToVictory then map AddToVictory locations else [])
        <> [ScenarioResolutionStep 1 resolution]
        <> [RecordCount YigsFury (yigsFury + vengeance)]
        <> [CrossOutRecord TheHarbingerIsStillAlive | inVictory]
        <> [RecordCount TheHarbingerIsStillAlive damage | not inVictory]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolutionStep 1 resolution -> do
      n <-
        selectCount
          $ VictoryDisplayCardMatch
          $ CardWithTrait
            Trait.Tenochtitlan
      gainXp <- toGainXp attrs $ getXpWithBonus n
      pushAll
        $ RecordCount PathsAreKnownToYou n
        : [ Record IchtacaHasConfidenceInYou
          | n >= 3 && resolution == Resolution 1
          ]
          <> gainXp
      pure s
    _ -> TheBoundaryBeyond <$> runMessage msg attrs
