{- | Against the Wendigo — a fan side story by Vinn Quest.

The valley is a 3x4 grid. The bottom row and the middle column are fixed; the
six Uncharted locations are shuffled into the two outer columns, so the map is
different every game and every connection has to be wired from the grid rather
than from printed symbols.
-}
module Arkham.Homebrew.AgainstTheWendigo.Scenarios.AgainstTheWendigo (againstTheWendigo) where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgainstTheWendigo.Helpers
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Homebrew.AgainstTheWendigo.ScenarioDeckKeys (pattern StudentsFateDeck)
import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Message.Lifted.Log
import Arkham.Helpers.Xp (XpBonus (NoBonus), toBonus)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Madness))

newtype AgainstTheWendigo = AgainstTheWendigo ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

againstTheWendigo :: Difficulty -> AgainstTheWendigo
againstTheWendigo difficulty =
  sideStory
    AgainstTheWendigo
    ":against-the-wendigo:001"
    "Against the Wendigo"
    difficulty
    [ "uncharted1 northHanninah3 uncharted2"
    , "uncharted3 northHanninah2 uncharted4"
    , "uncharted5 northHanninah1 uncharted6"
    , "sarceeTerritory jetty fortMcDonald"
    ]

instance HasChaosTokenValue AgainstTheWendigo where
  getChaosTokenValue iid chaosTokenFace (AgainstTheWendigo attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | The seven Uncharted locations. Setup removes one of the two Mountain Ranges
at random and deals the remaining six into the outer columns.
-}
unchartedLocations :: [CardDef]
unchartedLocations =
  [ Locations.templeOfIthaqua
  , Locations.madProspector
  , Locations.impenetrableForest
  , Locations.swamp
  , Locations.siteOfAncientStones
  , Locations.sinisterTaiga
  , Locations.hiddenHut
  ]

-- | Each student is printed twice; one of each pair is removed before play.
studentFatePairs :: [(CardDef, CardDef)]
studentFatePairs =
  [ (Stories.bernardsFateV1, Stories.bernardsFateV2)
  , (Stories.normansFateV1, Stories.normansFateV2)
  , (Stories.sylviasFateV1, Stories.sylviasFateV2)
  ]

instance RunMessage AgainstTheWendigo where
  runMessage msg s@(AgainstTheWendigo attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "prologue" do
      flavor $ h "title" >> p "body1" >> p "body2" >> p "body3"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup AgainstTheWendigo attrs do
      setup $ ul do
        li "gatherSets"
        li "setAsideWendigosMyth"
        li "placeStartingLocations"
        li "placeNorthHanninah"
        li "placeUncharted"
        li "studentsFateDeck"
        li "setAsideStories"
        li "setAsideAssets"
        unscoped $ li "shuffleRemainder"
        li "tabletToken"

      gather Set.HanninahValley
      gatherAndSetAside Set.WendigosMyth

      -- The bottom row: where the investigators start and where they can resign.
      jetty <- placeLabeled "jetty" Locations.jetty
      fort <- placeLabeled "fortMcDonald" Locations.fortMcDonald
      sarcee <- placeLabeled "sarceeTerritory" Locations.sarceeTerritory
      startAt jetty
      connectBothWays jetty fort
      connectBothWays jetty sarcee

      -- The middle column: the river running north out of the Jetty.
      shuffledRiver <-
        shuffleM [Locations.northHanninah1, Locations.northHanninah2, Locations.northHanninah3]
      river <-
        for (zip ["northHanninah1", "northHanninah2", "northHanninah3"] shuffledRiver)
          $ uncurry placeLabeled
      for_ (zip (jetty : river) river) (uncurry connectBothWays)

      -- The outer columns. One Mountain Range is removed at random; the rest are
      -- dealt one to the East and one to the West of each North Hanninah.
      (keptRange, removedRange) <-
        splitAt 1 <$> shuffleM [Locations.templeOfIthaqua, Locations.madProspector]
      removeEvery removedRange
      uncharted <- shuffleM (keptRange <> drop 2 unchartedLocations)
      let slots = ["uncharted" <> tshow (n :: Int) | n <- [1 .. 6]]
      outer <- for (zip slots uncharted) $ uncurry placeLabeled
      for_ (zip river (rowPairs outer)) \(mid, neighbours) ->
        for_ neighbours (connectBothWays mid)

      -- One of each pair of student fates survives; the three survivors are the deck.
      kept <- for studentFatePairs \(a, b) -> do
        (keep, drop') <- splitAt 1 <$> shuffleM [a, b]
        removeEvery drop'
        pure keep
      addExtraDeck StudentsFateDeck =<< shuffle . map toCard =<< traverse genCard (concat kept)

      setAside
        [ Stories.hanninahsGold
        , Stories.charlieFoxtailsDestiny
        , Stories.theKnowledgeOfTheCold
        , Assets.expeditionNotebook
        , Assets.sarceeGuide
        , Assets.tomahawk
        , Assets.ithaquasKnowledge
        , Treacheries.oldInjury
        ]

      -- "If you don't have a [tablet] token in your chaos bag, add one for this game."
      addChaosToken Tablet

      setAgendaDeck
        [ Agendas.aDarkAndDisturbingValley
        , Agendas.somethingDarkIsComing
        , Agendas.theWendigoHuntsYou
        ]
      setActDeck
        [ Acts.inSearchOfTheMissing
        , Acts.onTheStudentsTrack
        , Acts.northHanninahsMysteries
        ]
    {- | The scenario reference card: "[tablet]: If you succeed, place 1 clue
    from the reserve on the Mountain Range." Only this ever puts clues there, so
    the Mountain Range's own "Forced - if there are 2 clues (3 for a 3 or 4
    player game): reveal it" is checked here too. -}
    PassedSkillTestWithToken _ Tablet -> do
      ranges <- select $ LocationWithTitle "Mountain Range" <> UnrevealedLocation
      for_ (take 1 ranges) \lid -> do
        placeClues ScenarioSource lid 1
        n <- getPlayerCount
        clues <- field LocationClues lid
        when (clues >= if n >= 3 then 3 else 2) $ reveal lid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          flavor $ h "noResolution" >> p "noResolutionBody"
          resignedOrDefeatedTrauma attrs
          studentBranch
        Resolution 1 -> do
          flavor $ h "resolution1" >> p "resolution1Body"
          eachInvestigator (`sufferMentalTrauma` 1)
          record YouDefeatedTheWendigo
          studentBranch
        Resolution 2 -> do
          flavor $ h "resolution2" >> p "resolution2Body"
          eachInvestigator \iid -> push $ HealTrauma iid 0 1
          recordWendigoStillRoams
          awardScenarioXp attrs NoBonus
          epilogue
        Resolution 3 -> do
          flavor $ h "resolution3" >> p "resolution3Body"
          recordWendigoStillRoams
          awardScenarioXp attrs NoBonus
          epilogue
        Resolution 4 -> do
          flavor $ h "resolution4" >> p "resolution4Body"
          eachInvestigator \iid -> searchCollectionForRandomBasicWeakness iid attrs [Madness]
          recordWendigoStillRoams
          awardScenarioXp attrs $ toBonus "glimpsedTheMythos" 2
          epilogue
        _ -> error $ "Unknown resolution: " <> show r
      endOfScenario
      pure s
    _ -> AgainstTheWendigo <$> liftRunMessage msg attrs

{- | "Each investigator who resigned suffers 1 physical trauma. Each
investigator who was defeated by horror suffers 1 additional physical trauma.
Each investigator defeated by damage takes 1 Old Injury Weakness card and adds
it to their deck. Each investigator defeated by both damage and horror adds a
random Madness basic weakness as well."
-}
resignedOrDefeatedTrauma :: ReverseQueue m => ScenarioAttrs -> m ()
resignedOrDefeatedTrauma attrs = do
  selectEach ResignedInvestigator (`sufferPhysicalTrauma` 1)
  selectEach InsaneInvestigator (`sufferPhysicalTrauma` 1)
  selectEach KilledInvestigator \iid -> do
    addCampaignCardToDeck iid ShuffleIn Treacheries.oldInjury
    insane <- iid <=~> InsaneInvestigator
    when insane $ searchCollectionForRandomBasicWeakness iid attrs [Madness]

{- | The epilogue's four passages, then Dr. Nadelmann's fate. Each passage is
read only if the record it hangs off was made, and each one that hands a card
over does so here.
-}
epilogue :: (HasI18n, ReverseQueue m) => m ()
epilogue = scope "epilogue" do
  charlieSurvived <- selectAny $ assetIs Assets.charlieFoxtail
  when charlieSurvived do
    flavor $ h "charlie" >> p "charlieBody"
    investigators <- select UneliminatedInvestigator
    addCampaignCardToDeckChoice investigators ShuffleIn Assets.tomahawk

  whenHasRecord YouHaveFoundHanninahsGold do
    flavor $ h "gold" >> p "goldBody"
    investigators <- select UneliminatedInvestigator
    addCampaignCardToDeckChoice investigators ShuffleIn Assets.goldMiningRevenues

  whenHasRecord YouAreTheCustodianOfIthaquasKnowledge do
    flavor $ h "ithaqua" >> p "ithaquaBody"
    investigators <- select UneliminatedInvestigator
    addCampaignCardToDeckChoice investigators ShuffleIn Assets.ithaquasKnowledge

  -- Norman only counts as alive if he made it to the end still in play.
  normanSurvived <- selectAny $ assetIs Assets.normanFalkner
  if normanSurvived
    then do
      record NormanIsAlive
      flavor $ h "normanAlive" >> p "normanAliveBody"
    else whenHasRecord YouLetNormanDie do
      flavor $ h "normanDead" >> p "normanDeadBody"
      -- "Add a [tablet] token to the Chaos bag for the rest of your campaign."
      addChaosToken Tablet

  nadelmannsFate

{- | "Check your campaign log. If you defeated the Wendigo: read fate 1.
Otherwise, if you have enough evidence to clear Dr. Nadelmann: fate 2.
Otherwise, if you know that the Wendigo still roams: fate 3. Otherwise: fate 4."
-}
nadelmannsFate :: (HasI18n, ReverseQueue m) => m ()
nadelmannsFate = do
  defeatedWendigo <- getHasRecord YouDefeatedTheWendigo
  cleared <- getHasRecord YouHaveEnoughEvidenceToClearDrNadelmann
  stillRoams <- getHasRecord TheWendigoStillRoamsTheNorthHanninahValley
  let which
        | defeatedWendigo = "nadelmann1"
        | cleared = "nadelmann2"
        | stillRoams = "nadelmann3"
        | otherwise = "nadelmann4"
  flavor $ h which >> p (which <> "Body")

-- | Resolutions 2-4 are chosen by how many students' fates were discovered.
studentBranch :: ReverseQueue m => m ()
studentBranch = do
  discoveredAll <- getHasRecord YouHaveDiscoveredTheFateOfDrNadelmannsStudents
  discovered <-
    length
      <$> filterM
        getHasRecord
        [ YouHaveDiscoveredBernardsFate
        , YouHaveDiscoveredNormansFate
        , YouHaveDiscoveredSylviasFate
        ]
  push $ ScenarioResolution $ Resolution $ if discoveredAll || discovered == 3 then 2 else if discovered > 0 then 3 else 4

{- | "If the Wendigo was still in play at the end of the game, record that you
know that the Wendigo still roams the North Hanninah valley."
-}
recordWendigoStillRoams :: ReverseQueue m => m ()
recordWendigoStillRoams = do
  stillThere <- selectAny $ EnemyWithTitle "The Wendigo"
  when stillThere $ record TheWendigoStillRoamsTheNorthHanninahValley

{- | "Each investigator earns experience equal to the Victory X value of each card
in the victory display", plus a point each for Charlie and for the prospector.
-}
awardScenarioXp :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> XpBonus -> m ()
awardScenarioXp attrs extra = do
  savedCharlie <- getHasRecord YouSavedCharlie
  savedProspector <- getHasRecord YouSavedTheGoldProspector
  allGainXpWithBonus attrs
    $ mconcat
      $ [toBonus "savedCharlie" 1 | savedCharlie]
      <> [toBonus "savedTheGoldProspector" 1 | savedProspector]
      <> [extra]

-- | The six Uncharted locations, paired off one row at a time.
rowPairs :: [a] -> [[a]]
rowPairs (a : b : rest) = [a, b] : rowPairs rest
rowPairs xs = [xs | notNull xs]

chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne
    , PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFive
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusSeven
    , MinusEight
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
