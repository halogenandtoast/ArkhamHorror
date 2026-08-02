module Arkham.Scenario.Scenarios.ObsidianCanyons (obsidianCanyons) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDrownedCity.CampaignSteps (
  pattern CourtOfTheAncients,
  pattern SepulchreOfTheSleeper,
 )
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Card
import Arkham.ChaosToken
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreation (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Deck (ScenarioDeckKey (SummitDeck))
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Madness, Omen))
import Arkham.Treachery.Cards qualified as Treacheries

newtype ObsidianCanyons = ObsidianCanyons ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

obsidianCanyons :: Difficulty -> ObsidianCanyons
obsidianCanyons difficulty = scenario ObsidianCanyons "11639" "Obsidian Canyons" difficulty []

instance HasChaosTokenValue ObsidianCanyons where
  getChaosTokenValue iid chaosTokenFace (ObsidianCanyons attrs) = case chaosTokenFace of
    Skull -> do
      storm <- getStormIntensity
      pure $ ChaosTokenValue Skull (NegativeModifier $ byDifficulty attrs storm (storm + 2))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> do
      -- "-1 [-2] for each open sky adjacent to your current location." Adjacency
      -- in this scenario is the grid's, which is what the connection matcher
      -- resolves to; open sky is a location, so it is connected like any other.
      openSkies <- selectCount $ isOpenSky <> connectedFrom (locationWithInvestigator iid)
      pure $ ChaosTokenValue Tablet (NegativeModifier $ openSkies * byDifficulty attrs 1 2)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ObsidianCanyons where
  runMessage msg s@(ObsidianCanyons attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      storyWithContinue' do
        setTitle "title"
        p.basic "checkCampaignLog"
        ul do
          li.validate headedWest "headedWest"
          li.validate (not headedWest) "headedEast"

      investigators <- select (IncludeEliminated Anyone)
      withDreamsOfDestruction <-
        filterM (`investigatorHasTask` Assets.dreamsOfDestruction) investigators
      withProveYourWorth <- filterM (`investigatorHasTask` Assets.proveYourWorth) investigators
      flavor do
        setTitle "title"
        p $ if headedWest then "obsidianCanyons1" else "obsidianCanyons2"
      flavor do
        setTitle "title"
        p "obsidianCanyons3"
        ul do
          li.validate (notNull withDreamsOfDestruction || notNull withProveYourWorth) "resolveTasks"
          li.nested "checkCampaignLog" do
            li.validate headedWest "setupWest"
            li.validate (not headedWest) "setupEast"

      for_ withDreamsOfDestruction \iid ->
        storyWithChooseOneM'
          ( compose.green do
              h3 "dreamsOfDestruction.title"
              p "dreamsOfDestruction.instructions"
              p "dreamsOfDestruction.body"
              p "dreamsOfDestruction.vision"
              p "dreamsOfDestruction.sorrow"
              p.basic "dreamsOfDestruction.choose"
              ul do
                li "dreamsOfDestruction.drownOut"
                li "dreamsOfDestruction.letItIn"
          )
          do
            labeled' "dreamsOfDestruction.drownOut" do
              decrementRecordCountForInvestigator iid Key.DreamsOfDestruction 1
              sufferMentalTrauma iid 1
              -- "You (and only you) gain 2 bonus experience."
              gainXp iid attrs (ikey "xp.dreamsOfDestruction") 2
            labeled' "dreamsOfDestruction.letItIn" do
              incrementRecordCountForInvestigator iid Key.DreamsOfDestruction 2
              -- "In the Obsidian Canyons scenario" is this scenario, not the next
              -- one; the turn window expires on its own after each investigator's
              -- first turn, so it can only fire once apiece.
              for_ investigators \iid' -> turnModifier iid' attrs iid' dreamsOfDestruction

      for_ withProveYourWorth \iid ->
        storyWithChooseOneM'
          ( compose.green do
              h3 "proveYourWorth.title"
              p "proveYourWorth.instructions"
              p "proveYourWorth.body"
              p.basic "proveYourWorth.choose"
              ul do
                li "proveYourWorth.ropesAreWrong"
                li "proveYourWorth.trustTheirHandiwork"
          )
          do
            labeled' "proveYourWorth.ropesAreWrong" do
              -- "Choose an investigator to help (not yourself, if able)" — solo is
              -- the only case where you are still a legal choice.
              let others = filter (/= iid) investigators
              chooseOrRunOneM iid do
                questionLabeled' "proveYourWorth.chooseInvestigator"
                targets (if null others then [iid] else others) \chosen -> do
                  recordSetInsert Key.HelpedWithTheRopes [unInvestigatorId iid]
                  recordSetInsert Key.WasHelpedWithTheRopes [unInvestigatorId chosen]
              for_ investigators \iid' -> nextSetupModifier attrs.id attrs iid' (StartingResources (-2))
            labeled' "proveYourWorth.trustTheirHandiwork" do
              for_ investigators \iid' -> nextSetupModifier attrs.id attrs iid' (StartingResources 1)
      pure s
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents attrs.difficulty)
      pure s
    Setup -> runScenarioSetup ObsidianCanyons attrs do
      setUsesGrid
      -- Setup (v.I) is the *western* expedition and Setup (v.II) the eastern one.
      headedWest <- getHasRecord TheExpeditionHeadedWest
      creatureWasDefeated <- getHasRecord TheCreatureWasDefeated
      scope "setup" $ scope (if headedWest then "west" else "east") $ flavor do
        setTitle "title"
        ul do
          li "gatherSets"
          li.nested "gatherLocations" do
            li "removeLocations"
            li "setAsideLocations"
            li "buildSummitDeck"
            li "placeStart"
            li "fillGrid"
            li "shuffleInCentralSpire"
            li "beginAtRlyehStreets"
          li "windsStoryCard"
          li "buildActAgendaDeck"
          li "stormIntensity"
          li "starSpawn"
          li "setCardsAside"
          -- The two versions have their own scope, so each list is written out
          -- in full rather than validated against the other's entries.
          if headedWest then li "checkCampaignLogInescapable" else li "removeStarVampire"
          li "chooseExpeditionAsset"
          li "buildEncounterDeck"
          li "readyToBegin"

      additionalRules "openSky"
      additionalRules "summitDeck"
      additionalRules "stormIntensity"
      additionalRules "locationAdjacency"

      gather Set.ObsidianCanyons
      gather Set.CosmicLegacy
      gather Set.ElderMist
      gather Set.TheInescapable
      gather Set.Rlyeh
      gather Set.StarSpawn
      gather Set.ChillingCold
      gather Set.Nightgaunts
      gather Set.StrikingFear
      -- Only the western expedition uses Ancient Evils, which is why it has 14
      -- open sky cards to the eastern expedition's 11.
      when headedWest $ gather Set.AncientEvils

      -- Open Sky is a scenario-only location, but it still lives in the Obsidian
      -- Canyons encounter set, so 'gather' pulls a stray copy into the pool.
      removeCards =<< amongGathered (cardIs Locations.openSky)

      -- "Set all 14 [11] cards in the Ancient Evils, Chilling Cold, and Striking
      -- Fear encounter sets aside, out of play, as open sky." Their only game
      -- effect is being absent from the encounter deck, so they are consumed here
      -- and replaced one-for-one by the Open Sky location, which the grid, the
      -- Summit deck, and every "swap with an adjacent open sky" effect can handle
      -- uniformly.
      consumedForOpenSky <-
        fromGathered
          $ mapOneOf CardFromEncounterSet
          $ [Set.ChillingCold, Set.StrikingFear]
          <> [Set.AncientEvils | headedWest]
      openSkies <- genCards (replicate (length consumedForOpenSky) Locations.openSky)

      -- "Gather each location in the Obsidian Canyons encounter set." The named
      -- ones come out first; whatever is left is the Summit deck.
      rlyehStreetsCard <- fromGathered1 Locations.rlyehStreets
      centralSpireCard <- fromGathered1 Locations.centralSpire
      let removedLocations =
            if headedWest
              then [Locations.ancientDome]
              else [Locations.westernWall_11651, Locations.floatingSpire]
          asideLocations =
            if headedWest
              then
                [ Locations.floatingSpire
                , Locations.aerialWaterfall
                , Locations.glyphOrrery
                , Locations.westernWall_11651
                ]
              else [Locations.aerialWaterfall, Locations.glyphOrrery, Locations.ancientDome]
      -- 'fromGathered1' consumes them, which for these is the removal itself.
      traverse_ fromGathered1 removedLocations
      setAside =<< traverse fromGathered1 asideLocations
      summitCards <-
        shuffleM =<< fromGathered (CardFromEncounterSet Set.ObsidianCanyons <> #location)

      -- The act 1 diagram, 3 rows x 4 columns with R'lyeh Streets in the bottom
      -- left. Grid rows grow upward, so diagram row 3 is y = 0.
      rlyehStreets <- placeCardInGrid (Pos 0 0) rlyehStreetsCard
      startAt rlyehStreets
      let openSkyPositions = [Pos 1 0, Pos 0 1, Pos 2 1, Pos 1 2]
          fillPositions = [Pos 2 0, Pos 3 0, Pos 1 1, Pos 3 1, Pos 0 2, Pos 2 2, Pos 3 2]
      for_ (zip openSkyPositions openSkies) (uncurry placeCardInGrid_)
      setAside (drop (length openSkyPositions) openSkies)

      -- "Fill each empty space in the diagram with the bottom card of the Summit
      -- deck." Cards are always drawn from the bottom of this deck, so peel them
      -- off the end, last card first.
      let (summitRest, bottomCards) = splitAt (length summitCards - length fillPositions) summitCards
      for_ (zip fillPositions (reverse bottomCards)) (uncurry placeCardInGrid_)

      -- "Shuffle the set-aside Central Spire into the top three cards of the
      -- Summit deck" — it is the act 1 objective, so it must not be reachable
      -- until the grid has been worked through.
      let (topThree, deckRest) = splitAt 3 summitRest
      shuffledTop <- shuffleM (centralSpireCard : topThree)
      addExtraDeck SummitDeck (shuffledTop <> deckRest)

      -- v.I flies the Eastern Winds side, v.II the Western Winds side.
      placeStory $ if headedWest then Stories.easternWinds else Stories.westernWinds

      if headedWest
        then do
          setActDeck [Acts.scouringTheSpires, Acts.deadlySkies, Acts.returnToTheShoreline]
          setAgendaDeck [Agendas.encroachingStorms]
        else do
          setActDeck [Acts.searchingTheSpires, Acts.toTheAncientDome]
          setAgendaDeck [Agendas.otherworldlyStorms]

      -- "Place 1 resource on the scenario reference card under Storm Intensity."
      placeTokensOnScenarioReference Token.Resource 1

      starSpawn <- shuffleM =<< amongGathered (CardFromEncounterSet Set.StarSpawn <> #enemy)
      removeCards $ take (if headedWest then 2 else 3) starSpawn

      setAside [Assets.skyRelic, Treacheries.erodedFrieze, Assets.obsidianClaw]

      if headedWest
        then
          if creatureWasDefeated
            then removeCards =<< amongGathered (CardFromEncounterSet Set.TheInescapable)
            else do
              -- Open sky is a location for distance purposes, but the creature has
              -- to land somewhere investigators can reach.
              inescapable <- fromGathered1 Enemies.theInescapable
              createEnemyWith_ inescapable (FarthestLocationFromAll notOpenSky) \c ->
                c {enemyCreationExhausted = True}
        else do
          -- The eastern expedition never faces it here; the whole set sits out.
          setAsideEvery (CardFromEncounterSet Set.TheInescapable)
          -- "Search the Obsidian Canyons encounter set for one copy of the Star
          -- Vampire enemy and remove it from the game."
          removeCards . take 1 =<< amongGathered (cardIs Enemies.starVampire)

      eachInvestigator (`forInvestigator` Setup)
    ForInvestigator iid Setup -> do
      -- v.I offers an earned Artifact or an Expedition Item; v.II only the Item.
      headedWest <- getHasRecord TheExpeditionHeadedWest
      artifacts <- if headedWest then getAvailableArtifacts else pure []
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
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      -- Hard/expert places the doom on reveal; easy/standard only on a failure.
      placeDoomOnNearestEnemy attrs iid
      pure s
    FailedSkillTestWithToken iid Cultist | isEasyStandard attrs -> do
      placeDoomOnNearestEnemy attrs iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      whenAny (enemyAtLocationWith iid) $ drawAnotherChaosToken iid
      pure s
    RemovedLocation _ -> do
      -- "If a gap between locations and/or open sky is created for any reason and
      -- not immediately filled via scenario card instructions, fill that gap with
      -- the bottom card of the Summit deck." Winds and skyline rebuilds already
      -- have their slides/replacements queued, so the generic rule must not fill
      -- the just-vacated edge before those instructions execute.
      unlessM skylineInstructionsPending $ doStep 1 msg
      pure s
    DoStep 1 (RemovedLocation _) -> do
      fillSkylineGaps
      pure s
    BeginTurn iid -> do
      -- "Let it in": at the beginning of each investigator's first turn, they must
      -- draw the top card of the encounter deck.
      whenM (hasModifier iid dreamsOfDestruction) $ drawEncounterCard iid attrs
      pure s
    ScenarioSpecific "increaseStormIntensity" _ -> do
      placeTokens attrs ScenarioTarget #resource 1
      pure s
    ScenarioSpecific "decreaseStormIntensity" _ -> do
      storm <- getStormIntensity
      removeTokens attrs ScenarioTarget #resource (min 1 storm)
      pure s
    ScenarioSpecific "shuffleScouringAct2Summit" _ -> do
      cards <-
        getSetAsideCardsMatching
          $ mapOneOf cardIs [Locations.floatingSpire, Locations.aerialWaterfall]
      shuffleIntoSummitTop 3 cards
      pure s
    ScenarioSpecific "shuffleSearchingAct2Summit" _ -> do
      cards <-
        getSetAsideCardsMatching
          $ mapOneOf
            cardIs
            [Locations.ancientDome, Locations.aerialWaterfall, Locations.openSky]
      shuffleIntoSummitTop 5 cards
      pure s
    ScenarioSpecific "shuffleAct3Summit" _ -> do
      cards <- getSetAsideCardsMatching $ cardIs Locations.westernWall_11651
      shuffleIntoSummitTop 3 cards
      pure s
    ScenarioResolution res -> scope "resolutions" do
      headedWest <- getHasRecord TheExpeditionHeadedWest
      -- Shared by every resolution.
      crossOutRecordSetEntries RlyehMap [toJSON RlyehObsidianCanyons]

      -- "If the Obsidian Claw was under the control of an investigator when the
      -- scenario ended" — either face counts, since it can be flipped to (Power).
      let claimObsidianClaw =
            whenM
              ( selectAny
                  $ mapOneOf assetIs [Assets.obsidianClaw, Assets.obsidianClawPower]
                  <> AssetControlledBy Anyone
              )
              $ record ObsidianClaw

      -- "If any investigator has the Prove Your Worth Task, they resolve their
      -- respective story on this page." Every resolution reads it.
      let resolveProveYourWorth = do
            withProveYourWorth <-
              filterM (`investigatorHasTask` Assets.proveYourWorth)
                =<< select (IncludeEliminated Anyone)
            for_ withProveYourWorth \iid -> do
              helpers <- getSomeRecordSet @CardCode Key.HelpedWithTheRopes
              let helped = unInvestigatorId iid `elem` helpers
              scope "proveYourWorth" $ flavor $ compose.green do
                h3 "title"
                p "instructions"
                p.basic "checkCampaignLog"
                ul do
                  li.validate helped "helpedWithTheRopes"
                  li.validate (not helped) "otherwise"
                if helped
                  then do
                    p "task1"
                    ul do
                      li "task1Chosen"
                      li "task1Progress"
                  else do
                    p "task2"
                    ul do
                      li "task2Trauma"
                      li "task2OtherInvestigators"
              if helped
                then do
                  -- "The investigator you chose to help earlier suffers 1
                  -- physical trauma and gains 2 bonus experience."
                  chosen <- getSomeRecordSet @CardCode Key.WasHelpedWithTheRopes
                  helpedInvestigators <-
                    select
                      $ IncludeEliminated
                      $ mapOneOf (InvestigatorWithId . InvestigatorId) chosen
                  for_ helpedInvestigators \iid' -> do
                    sufferPhysicalTrauma iid' 1
                    gainXp iid' attrs (ikey "proveYourWorth.xp.helped") 2
                  incrementRecordCountForInvestigator iid Key.ProveYourWorth 2
                else do
                  decrementRecordCountForInvestigator iid Key.ProveYourWorth 1
                  sufferMentalTrauma iid 1
                  others <- select $ IncludeEliminated $ not_ (InvestigatorWithId iid)
                  for_ others \iid' -> gainXp iid' attrs (ikey "proveYourWorth.xp.other") 1

      case res of
        Resolution 1 -> do
          claimObsidianClaw
          resolutionWithXp "resolution1" $ allGainXp' attrs
          resolveProveYourWorth
          endOfScenarioThen SepulchreOfTheSleeper
        Resolution 2 -> do
          claimObsidianClaw
          resolutionWithXp "resolution2" $ allGainXp' attrs
          resolveProveYourWorth
          endOfScenarioThen CourtOfTheAncients
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXp' attrs
          -- "Each investigator must search the collection for a random Madness or
          -- Omen basic weakness and add it to their deck for the remainder of the
          -- campaign."
          eachInvestigator \iid -> searchCollectionForRandomBasicWeakness iid attrs [Madness, Omen]
          resolveProveYourWorth
          endOfScenarioThen $ if headedWest then SepulchreOfTheSleeper else CourtOfTheAncients
        _ -> error $ "Unknown resolution: " <> show res
      pure s
    _ -> ObsidianCanyons <$> liftRunMessage msg attrs
