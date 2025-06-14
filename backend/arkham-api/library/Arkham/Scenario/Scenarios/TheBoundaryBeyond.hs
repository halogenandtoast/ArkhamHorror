module Arkham.Scenario.Scenarios.TheBoundaryBeyond (setupTheBoundaryBeyond, theBoundaryBeyond, TheBoundaryBeyond (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Act
import Arkham.Helpers.Campaign
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers hiding (setupModifier)
import Arkham.Helpers.Scenario hiding (getIsReturnTo)
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (LocationName))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted hiding (EnemyDamage)
import Arkham.Scenarios.TheBoundaryBeyond.Helpers
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
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

setupTheBoundaryBeyond :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheBoundaryBeyond attrs = do
  setup do
    ul do
      li "gatherSets"
      li "placeLocations"
      li "explorationDeck"
      li "setAside"
      li "setActAndAgenda3OutOfPlay"
      li "poisoned"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToTheBoundaryBeyond
  gather Set.TheBoundaryBeyond
  gather Set.TemporalFlux `orWhenReturnTo` gather Set.TemporalHunters
  gather Set.Poison

  tokens <- getBagChaosTokens
  let
    cultistCount = count ((== Cultist) . chaosTokenFace) tokens
    tabletCount = count ((== Tablet) . chaosTokenFace) tokens

  when (cultistCount >= 2) do
    gather Set.PnakoticBrotherhood
    gather Set.DarkCult `orWhenReturnTo` gather Set.CultOfPnakotus

  when (tabletCount >= 2) do
    gather Set.YigsVenom `orWhenReturnTo` gather Set.VenomousHate
    gather Set.GuardiansOfTime

  when (cultistCount < 2 && tabletCount < 2) do
    gather Set.PnakoticBrotherhood
    gather Set.GuardiansOfTime

  zocalo <- place =<< Locations.zocalo `orSampleIfReturnTo` [Locations.returnToZocalo]
  coyoacan <- place =<< Locations.coyoacan `orSampleIfReturnTo` [Locations.returnToCoyoacan]
  place_
    =<< Locations.metropolitanCathedral
    `orSampleIfReturnTo` [Locations.returnToMetropolitanCathedral]
  place_ =<< Locations.templeRuins `orSampleIfReturnTo` [Locations.returnToTempleRuins]
  place_ =<< Locations.xochimilco `orSampleIfReturnTo` [Locations.returnToXochimilco]
  place_ =<< Locations.chapultepecPark `orSampleIfReturnTo` [Locations.returnToChapultepecPark]

  isReturnTo <- getIsReturnTo
  let treacheries =
        guard (not isReturnTo)
          *> [ Treacheries.windowToAnotherTime
             , Treacheries.timelineDestabilization
             , Treacheries.aTearInTime
             , Treacheries.lostInTime
             ]

  addExtraDeck ExplorationDeck
    =<< shuffle
      ( [ Locations.temploMayor_174
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
          <> treacheries
      )

  setAsidePoisonedCount <- getSetAsidePoisonedCount
  setAside
    $ [Enemies.padmaAmrita, Acts.theReturnTrip, Agendas.timeCollapsing]
    <> replicate setAsidePoisonedCount Treacheries.poisoned

  whenReturnTo $ setAside [Enemies.harbingerOfValusiaTheSleeperReturns]
  eachInvestigator \iid -> chooseTargetM iid [zocalo, coyoacan] $ moveTo_ attrs iid
  setAgendaDeck [Agendas.theBoundaryBroken, Agendas.theBarrierIsThin]
  setActDeck [Acts.crossingTheThreshold, Acts.pastAndPresent]

instance RunMessage TheBoundaryBeyond where
  runMessage msg s@(TheBoundaryBeyond attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "part1"

      unlessStandalone do
        forgedABondWithIchtaca <- getHasRecord TheInvestigatorsForgedABondWithIchtaca
        flavor do
          h "title"
          compose.green $ scope "ichtaca" do
            p "instructions"
            p.validate forgedABondWithIchtaca "ichtacasQuest"
            p.validate (not forgedABondWithIchtaca) "silentJourney"

        unless forgedABondWithIchtaca do
          eachInvestigator \iid -> setupModifier attrs iid (StartingHand (-2))

        foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic

        flavor do
          h "title"
          compose.green $ scope "missingRelic" do
            p "instructions"
            p.validate foundTheMissingRelic "arcaneThrumming"
            p.validate (not foundTheMissingRelic) "growingConcern"

        when foundTheMissingRelic do
          removeCampaignCard Assets.relicOfAgesADeviceOfSomeSort
          withOwner Assets.relicOfAgesADeviceOfSomeSort \owner -> do
            addCampaignCardToDeck owner ShuffleIn Assets.relicOfAgesForestallingTheFuture

        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro

        flavor do
          h "title"
          compose.green $ scope "alejandro" do
            p "instructions"
            p.validate rescuedAlejandro "alejandrosThoughts"
            p.validate (not rescuedAlejandro) "anEmptySeat"

        when rescuedAlejandro do
          eachInvestigator \iid -> setupModifier attrs iid (StartingResources 2)

        withGasoline <- headMay <$> getInvestigatorsWithSupply Gasoline

        flavor do
          h "title"
          compose.green $ scope "gas" do
            p "instructions"
            p.validate (isNothing withGasoline) "outOfGas"

        when (isNothing withGasoline) do
          eachInvestigator \iid -> setupModifier attrs iid CannotMulligan
        for_ withGasoline \iid -> push $ UseSupply iid Gasoline

      flavor $ h "title" >> p "part2"
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup TheBoundaryBeyond attrs $ setupTheBoundaryBeyond attrs
    Explore iid _ _ -> do
      checkWhen $ Window.AttemptExplore iid
      push $ Do msg
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
      push $ ReplaceLocation lid card Swap
      pure s
    ResolveChaosToken _ chaosTokenFace iid | chaosTokenFace `elem` [Cultist, Tablet] -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      ls <- select $ NearestLocationTo iid $ LocationWithTrait Trait.Ancient
      chooseTargetM iid ls \target -> placeTokens ElderThing target Clue 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          ls <- select $ EnemyWithTrait Trait.Cultist
          if isEasyStandard attrs
            then chooseTargetM iid ls \target -> placeDoom Cultist target 1
            else for_ ls \t -> placeDoom Cultist t 1
        Tablet -> do
          serpents <- select $ EnemyWithTrait Trait.Serpent <> at_ (locationWithInvestigator iid)
          if isEasyStandard attrs
            then chooseTargetM iid serpents \serpent -> initiateEnemyAttack serpent attrs iid
            else for_ serpents \serpent -> initiateEnemyAttack serpent attrs iid
        ElderThing | isEasyStandard attrs -> do
          ls <- select $ NearestLocationTo iid $ LocationWithTrait Trait.Ancient
          chooseTargetM iid ls \target -> placeTokens ElderThing target Clue 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      step <- getCurrentActStep
      locations <- selectTargets $ LocationWithTrait Trait.Tenochtitlan <> LocationWithoutClues

      let
        addLocationsToVictory =
          and [step == 2, notNull locations, r `elem` [NoResolution, Resolution 2]]

      resolution $ case r of
        NoResolution -> "noResolution"
        Resolution 1 -> "resolution1"
        Resolution 2 -> "resolution2"
        _ -> error "invalid resolution"
      when addLocationsToVictory do
        for_ locations addToVictory
      doStep 1 msg

      vengeance <- getTotalVengeanceInVictoryDisplay
      yigsFury <- getRecordCount YigsFury
      recordCount YigsFury (yigsFury + vengeance)

      inVictory <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.harbingerOfValusia
      if inVictory
        then crossOut TheHarbingerIsStillAlive
        else do
          inPlayHarbinger <- selectOne $ enemyIs Enemies.harbingerOfValusia
          damage <- case inPlayHarbinger of
            Just eid -> field EnemyDamage eid
            Nothing -> getRecordCount TheHarbingerIsStillAlive
          recordCount TheHarbingerIsStillAlive damage
      endOfScenario
      pure s
    DoStep 1 (ScenarioResolution r) -> scope "resolutions" do
      n <- selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTrait Trait.Tenochtitlan
      recordCount PathsAreKnownToYou n
      recordWhen (n >= 3 && r == Resolution 1) IchtacaHasConfidenceInYou
      allGainXpWithBonus attrs $ toBonus "additional" n
      pure s
    _ -> TheBoundaryBeyond <$> liftRunMessage msg attrs
