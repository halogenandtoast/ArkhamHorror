module Arkham.Scenario.Scenarios.TheApiary (theApiary) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetDoom))
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (
  pattern TheDrownedQuarter,
  pattern TheGrandVault,
  pattern TheWesternWall,
 )
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDoom))
import Arkham.Helpers
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Investigator.Cards (ursulaDowns)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (GridLocation (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (setAsideCardsL)
import Arkham.Scenarios.TheApiary.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheApiary = TheApiary ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theApiary :: Difficulty -> TheApiary
theApiary difficulty = scenario TheApiary "11553" "The Apiary" difficulty []

instance HasChaosTokenValue TheApiary where
  getChaosTokenValue iid chaosTokenFace (TheApiary attrs) = case chaosTokenFace of
    Skull -> do
      enemyDoom <- selectSum EnemyDoom AnyEnemy
      assetDoom <- selectSum AssetDoom AnyAsset
      let total = enemyDoom + assetDoom
      pure
        $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs (min 4 (total `div` 2)) (min 8 total))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheApiary where
  runMessage msg s@(TheApiary attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      isUrsula <- selectAny $ investigatorIs ursulaDowns
      withWalkInFaith <-
        filterM (`investigatorHasTask` Assets.walkInFaith)
          =<< select (IncludeEliminated Anyone)
      flavor do
        setTitle "title"
        if headedWest
          then do
            p "apiary1"
            p.validate isUrsula "ursulaDowns"
            p "apiary1Andy"
            p "apiary1Tremor"
            p "apiary1Cavern"
            p "apiary1Conclusion"
          else do
            p "apiary2"
            p "apiary2Ruby"
            p "apiary2Cavern"
            p "apiary2Note"
            p "apiary2Conclusion"
        ul do
          unscoped
            $ withVars ["token" .= String (if headedWest then "tablet" else "cultist")]
            $ li "addToken"
          li.validate (notNull withWalkInFaith) "resolveWalkInFaith"
        p.basic $ if headedWest then "proceedToWesternSetup" else "proceedToEasternSetup"

      -- The campaign handles AddChaosToken by adding to its own bag, so this
      -- sticks for the remainder of the campaign and not just this scenario.
      addChaosToken (if headedWest then Tablet else Cultist)

      for_ withWalkInFaith \iid ->
        storyWithChooseOneM'
          ( compose.green do
              h3 "walkInFaith.title"
              p "walkInFaith.instructions"
              p "walkInFaith.body"
              p.basic "walkInFaith.choose"
              ul do
                li "walkInFaith.doubts"
                li "walkInFaith.resolve"
          )
          do
            -- TODO: both branches also modify the first encounter-deck draw of
            -- each investigator during this scenario, which The Apiary itself
            -- does not implement yet.
            labeled' "walkInFaith.doubts" $ decrementRecordCountForInvestigator iid Key.WalkInFaith 1
            labeled' "walkInFaith.resolve" do
              incrementRecordCountForInvestigator iid Key.WalkInFaith 2
              sufferMentalTrauma iid 1
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup TheApiary attrs do
      setUsesGrid
      headedWest <- getHasRecord TheExpeditionHeadedWest
      creatureWasDefeated <- getHasRecord TheCreatureWasDefeated
      -- Bind the scope name rather than a partially applied `scope`, which the
      -- monomorphism restriction would pin to a single builder type.
      let version = if headedWest then "west" else "east" :: Text
      scope "setup" $ flavor do
        scope version $ setTitle "title"
        ul do
          scope version $ li "gatherSets"
          li.nested "placeApiaryEntrance" $ li "beginAtApiaryEntrance"
          scope version $ li "buildActDeck"
          scope version $ li "removeFromGame"
          scope version $ li "setCardsAside"
          li "chooseExpeditionAsset"
          unless headedWest $ li.validate creatureWasDefeated "removeTheInescapable"
          li.nested "buildEncounterDeck" $ scope version $ li "shuffleIntoBottomTen"
          li "readyToBegin"

      gather Set.TheApiary
      gather Set.CosmicLegacy
      gather Set.ElderMist
      gather Set.TheInescapable
      gather Set.StrikingFear
      if headedWest
        then do
          -- The pilgrims path: Lost Pilgrims act, Mother removed.
          gather Set.Pilgrims
          gather Set.DarkCult
        else do
          -- The hive-mind path: The Hive Mind act, Maria Rivera removed. Only the
          -- Infected Star Spawn is used; the rest of that set is removed from the game.
          gatherJust Set.StarSpawn [Enemies.infectedStarSpawn]
          gather Set.Stowaways

      setActDeck [Acts.unsettlingSigns, if headedWest then Acts.lostPilgrims else Acts.theHiveMind]
      setAgendaDeck [Agendas.stirringInTheDark, Agendas.loathsomeParasites]

      removeCards
        =<< amongGathered (cardIs $ if headedWest then Enemies.mother else Assets.mariaRivera)

      -- Every Apiary location enters via its own Revelation, so set them all aside.
      -- Growing Fields and both Fleshy Paths are the exception: they are shuffled
      -- back into the encounter deck, so they simply stay in the gathered cards.
      -- The story enemies/assets are likewise set aside; the Central Chamber and its
      -- 4-location ring form as exploration uncovers them.
      setAside
        $ [ Locations.churningChasm
          , Locations.corruptedVault
          , Locations.luminousTunnels
          , Locations.spawningGrounds
          , Locations.lostCampsite
          , Locations.graspingCorridor
          , Locations.starvingCorridor
          , Locations.acidicCoelom
          , Locations.centralChamber
          , Locations.hiddenVault
          , Enemies.grotesqueAmalgam
          , Enemies.squamousParasite
          , Assets.ancientRelic
          , Assets.grislyMask
          ]
        <> [if headedWest then Assets.mariaRivera else Enemies.mother]
      setAside =<< amongGathered (cardIs Treacheries.parasiticTransformation)

      if headedWest
        then do
          setAside =<< amongGathered (CardFromEncounterSet Set.Pilgrims)
          setAside =<< amongGathered (CardFromEncounterSet Set.TheInescapable)
        else do
          -- The eastern expedition leaves The Inescapable in the encounter deck,
          -- unless the creature has already been dealt with.
          when creatureWasDefeated do
            removeCards =<< amongGathered (CardFromEncounterSet Set.TheInescapable)

      apiaryEntrance <- place Locations.apiaryEntranceBeckoningLight
      startAt apiaryEntrance
      setScenarioMeta initApiaryMeta
      eachInvestigator (`forInvestigator` Setup)
    -- Every location enters play from the encounter deck, so each one takes its
    -- map position as it is placed rather than during setup. See 'apiaryPositions'.
    PlacedLocation _ (apiaryPosition -> Just pos) lid -> do
      push $ PlaceGrid (GridLocation pos lid)
      pure s
    ForInvestigator iid Setup -> do
      artifacts <- getAvailableArtifacts
      chooseOneM iid do
        questionLabeled' "chooseExpeditionAssetQuestion"
        labeled' "noExpeditionAsset" nothing
        for_ (artifacts <> expeditionItems) \asset ->
          cardLabeled asset.cardCode $ handleTarget iid attrs (CardCodeTarget asset.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- EncounterCard <$> genEncounterCard def
        createAssetAt_ card (InPlayArea iid)
      pure s
    EndSetup -> do
      -- The final setup shuffle has already happened, so the hidden location has to
      -- be worked into the bottom of the finished deck here rather than during setup.
      headedWest <- getHasRecord TheExpeditionHeadedWest
      let hiddenLocation = if headedWest then Locations.luminousTunnels else Locations.graspingCorridor
      cards <- getSetAsideCardsMatching (cardIs hiddenLocation)
      deck <- unDeck <$> getEncounterDeck
      let (rest, bottomTen) = splitAt (max 0 (length deck - 10)) deck
      shuffled <- shuffle (onlyEncounterCards cards <> bottomTen)
      -- Written straight onto the attrs: the base handler pushes BeginGame, which
      -- would otherwise be pulled ahead of a queued SetEncounterDeck.
      TheApiary
        . (encounterDeckL .~ Deck (rest <> shuffled))
        . (setAsideCardsL %~ filter (`notElem` cards))
        <$> liftRunMessage msg attrs
    -- "If there is a Cultist enemy at your location, reveal another token."
    ResolveChaosToken _ Cultist iid -> do
      whenM (selectAny $ #cultist <> enemyAtLocationWith iid)
        $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        -- "Place 1 doom on an enemy at your location with no doom on it." On hard and
        -- expert the no-doom enemy is only a preference ("if able"), so any enemy
        -- there will do once they all have doom.
        Tablet -> do
          withoutDoom <- select $ EnemyWithoutDoom <> enemyAtLocationWith iid
          enemies <-
            if notNull withoutDoom || isEasyStandard attrs
              then pure withoutDoom
              else select $ enemyAtLocationWith iid
          unless (null enemies) $ chooseTargetM iid enemies \eid -> placeDoom Tablet eid 1
        ElderThing -> chooseOneM iid $ unscoped $ countVar 1 do
          labeled' "takeDamage" $ assignDamage iid ElderThing 1
          labeled' "takeHorror" $ assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    -- The Hive Mind act flips a coin each round end and rotates the Central
    -- Chamber; the rotation changes which ring location it "faces" (= connects to).
    ScenarioSpecific "rotateCentralChamber" v -> do
      facing <- getCentralChamberFacing
      let dir = toResultDefault ("clockwise" :: Text) v
      let rotate = if dir == "clockwise" then rotateFacingClockwise else rotateFacingCounterClockwise
      setScenarioMeta (ApiaryMeta (rotate facing))
      pure s
    -- Resolution 4 is only ever reached from resolution 3 or from no resolution, so
    -- it must not re-run the shared bookkeeping (or the defeat story) below.
    ScenarioResolution (Resolution 4) -> scope "resolutions" do
      storyWithChooseOneM'
        (compose.resolution $ scope "resolution4" $ setTitle "title" >> p "body")
        do
          labeled' "resolution4.drownedQuarter" $ endOfScenarioThen TheDrownedQuarter
          labeled' "resolution4.westernWall" $ endOfScenarioThen TheWesternWall
      pure s
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest

      -- "Before resolving any other resolution, if at least 1 investigator was
      -- defeated: The defeated investigators read Investigator Defeat first."
      defeated <- select DefeatedInvestigator
      -- Anyone not defeated here and still alive carries the campaign on; resigning
      -- eliminates an investigator but does not kill them.
      survivors <-
        select
          $ IncludeEliminated
          $ not_ (mapOneOf InvestigatorWithId defeated)
          <> not_ KilledInvestigator
          <> not_ InsaneInvestigator
      unless (null defeated) do
        resolutionOnly defeated $ scope "investigatorDefeat" do
          setTitle "title"
          p "body"
          ul do
            li.nested "killed" $ li "storyAssets"
            li.validate (notNull survivors) "otherResolution"
        -- "If the Andy Van Nortwick or Ruby Standish story asset was in the deck of
        -- any investigator who was killed, choose a surviving investigator to add
        -- them to their deck for the remainder of the campaign."
        unless (null survivors) do
          storyCards <- getCampaignStoryCards
          for_ defeated \iid ->
            for_ (findWithDefault [] iid storyCards) \card ->
              when (toCardDef card `elem` [Assets.andyVanNortwick, Assets.rubyStandish])
                $ addCampaignCardToDeckChoice survivors DoNotShuffleIn card
        -- "Each defeated investigator is killed."
        for_ defeated $ kill attrs

      -- "If there are no surviving investigators to continue the campaign, the
      -- investigators lose the campaign." Only *then* do "the remaining
      -- investigators proceed to that resolution", so stop here when there are none.
      -- kill's own CheckForRemainingInvestigators cannot do this for us: it is a
      -- no-op once the scenario is in its resolution.
      if null survivors
        then gameOver
        else resolveTheApiary res headedWest attrs
      pure s
    _ -> TheApiary <$> liftRunMessage msg attrs

{- | The body of every resolution but Investigator Defeat, which decides on its own
whether we get this far.
-}
resolveTheApiary
  :: (HasI18n, ReverseQueue m) => Resolution -> Bool -> ScenarioAttrs -> m ()
resolveTheApiary res headedWest attrs = do
  -- "Each investigator must search The Apiary encounter set for a copy of Parasitic
  -- Transformation and add it to their deck." Generated rather than fetched: setup
  -- sets the printed copies aside, so fetching one hands back the *encounter* card,
  -- and ReloadDecks only puts player cards from the campaign's story cards into a
  -- deck, so it would silently never arrive.
  let addParasiticTransformation = eachInvestigator \iid ->
        addCampaignCardToDeck iid ShuffleIn
          =<< genCard Treacheries.parasiticTransformation
  -- Shared by every resolution: cross The Apiary off the R'lyeh map, and earn the
  -- Grisly "Mask" if an investigator still controlled it when the scenario ended.
  earnedGrislyMask <- selectAny $ assetIs Assets.grislyMask <> AssetControlledBy Anyone
  crossOutRecordSetEntries RlyehMap [toJSON RlyehApiary]
  when earnedGrislyMask $ record GrislyMask

  case res of
    Resolution 1 -> do
      record ThePilgrimsWereSaved
      xp <- allGainXpWithBonus' attrs $ toBonus "bonus" 2
      resolutionFlavor $ withVars ["xp" .= xp] $ scope "resolution1" do
        setTitle "title"
        p "body"
        ul do
          li.nested "updateCampaignLog" do
            li "recordPilgrimsWereSaved"
            li "crossOutTheApiary"
            li.validate earnedGrislyMask "grislyMask"
          li.nested "victory" do
            li "experience"
            li "bonus"
          li "proceedToTheGrandVault"
      endOfScenarioThen TheGrandVault
    Resolution 2 -> do
      record ThePilgrimsWereDevoured
      addParasiticTransformation
      xp <- allGainXp' attrs
      resolutionFlavor $ withVars ["xp" .= xp] $ scope "resolution2" do
        setTitle "title"
        p "body"
        ul do
          li "parasiticTransformation"
          li.nested "updateCampaignLog" do
            li "recordPilgrimsWereDevoured"
            li "crossOutTheApiary"
            li.validate earnedGrislyMask "grislyMask"
          li.nested "victory" $ li "experience"
          li "proceedToTheGrandVault"
      endOfScenarioThen TheGrandVault
    Resolution 3 -> do
      record TheInvestigatorsExterminatedTheAlienParasites
      xp <- allGainXp' attrs
      resolutionFlavor $ withVars ["xp" .= xp] $ scope "resolution3" do
        setTitle "title"
        p "body"
        ul do
          li.nested "updateCampaignLog" do
            li "recordParasitesExterminated"
            li "crossOutTheApiary"
            li.validate earnedGrislyMask "grislyMask"
          li.nested "victory" $ li "experience"
          li "proceedToResolution4"
      push R4
    NoResolution -> do
      addParasiticTransformation
      xp <- allGainXp' attrs
      resolutionFlavor $ withVars ["xp" .= xp] $ scope "noResolution" do
        setTitle "title"
        p "body"
        ul do
          li "parasiticTransformation"
          li.nested "updateCampaignLog" do
            li "crossOutTheApiary"
            li.validate earnedGrislyMask "grislyMask"
          li.nested "victory" $ li "experience"
          li.nested "checkCampaignLog" do
            li.validate headedWest "proceedToTheGrandVault"
            li.validate (not headedWest) "proceedToResolution4"
      if headedWest then endOfScenarioThen TheGrandVault else push R4
    _ -> error $ "Unknown resolution: " <> show res
