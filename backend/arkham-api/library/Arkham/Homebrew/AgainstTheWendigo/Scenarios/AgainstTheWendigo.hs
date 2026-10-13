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
import Arkham.Helpers.Xp (XpBonus (NoBonus), toBonus)
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
import Arkham.Location.Grid (Pos (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Ally, Madness))

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
    -- Every location is placed in the grid, which regenerates the layout as it
    -- goes (@PlaceGrid@), so there is nothing to declare here.
    []

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
    ScenarioSpecific "navigate.step" val -> do
      logNavigateStep val
      pure s
    PreScenarioSetup -> scope "prologue" do
      flavor $ h "title" >> p "body"
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

      {- The valley is laid out in the grid: x runs West to East with the river at
      0, y runs South to North with the bottom row at 0. Placing a location
      there is what tells the engine which locations are printed next to which
      -- the compass directions every card in this scenario talks about. -}
      jetty <- placeInGrid (Pos 0 0) Locations.jetty
      placeInGrid_ (Pos 1 0) Locations.fortMcDonald
      placeInGrid_ (Pos (-1) 0) Locations.sarceeTerritory
      startAt jetty

      -- The middle column: the river running north out of the Jetty. The slots
      -- are numbered up from the Jetty, so northHanninah1 is the southernmost
      -- and the map reads these three in reverse.
      shuffledRiver <-
        shuffleM [Locations.northHanninah1, Locations.northHanninah2, Locations.northHanninah3]
      for_ (zip [1 .. 3] shuffledRiver) \(y, def) -> placeInGrid_ (Pos 0 y) def

      -- The outer columns. One Mountain Range is removed at random; the rest are
      -- dealt one to the East and one to the West of each North Hanninah.
      (keptRange, removedRange) <-
        splitAt 1 <$> shuffleM [Locations.templeOfIthaqua, Locations.madProspector]
      removeEvery removedRange
      uncharted <- shuffleM (keptRange <> drop 2 unchartedLocations)
      {- West then East of each river location, from the northernmost row down.
      Nothing is wired by hand: every card prints which compass neighbours it
      reaches, and the grid is what answers that. -}
      let cells = [Pos x y | y <- [3, 2, 1], x <- [-1, 1]]
      for_ (zip cells uncharted) (uncurry placeInGrid_)

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
      addChaosTokenForGame Tablet

      setAgendaDeck
        [ Agendas.aDarkAndDisturbingValley
        , Agendas.somethingDarkIsComing
        , Agendas.theWendigoHuntsYou
        , Agendas.theWendigosAttack
        ]
      setActDeck
        [ Acts.inSearchOfTheMissing
        , Acts.onTheStudentsTrack
        , Acts.northHanninahsMysteries
        ]
    {- The scenario reference card: "[tablet]: If you succeed, place 1 clue from
    the reserve on the Mountain Range." Only this ever puts clues there. The
    reveal at 2 clues (3 for a 3 or 4 player game) is the Mountain Range's own
    Forced ability; see 'mountainRangeReveal'. -}
    PassedSkillTestWithToken _ Tablet -> do
      ranges <- select $ LocationWithTitle "Mountain Range" <> UnrevealedLocation
      for_ (take 1 ranges) \lid -> placeClues ScenarioSource lid 1
      pure s
    -- "[skull]: If you fail, lose 1 resource." Hard/expert loses 2.
    FailedSkillTestWithToken iid Skull -> do
      loseResources iid Skull (if isEasyStandard attrs then 1 else 2)
      pure s
    {- "[cultist]: If you fail, an Ally asset you control takes 1 damage and 1
    horror." Hard/expert removes one from the game instead. -}
    FailedSkillTestWithToken iid Cultist -> do
      allies <- select $ AssetWithTrait Ally <> assetControlledBy iid
      unless (null allies) $ chooseTargetM iid allies \ally ->
        if isEasyStandard attrs
          then push $ DealAssetDamageWithCheck ally (toSource Cultist) 1 1 True
          else removeFromGame ally
      pure s
    -- "[tablet]: ... If you fail, take 1 horror."
    FailedSkillTestWithToken iid Tablet -> do
      assignHorror iid Tablet 1
      pure s
    -- "[elder thing]: If you fail, take 1 horror." Hard/expert adds 1 damage.
    FailedSkillTestWithToken iid ElderThing -> do
      if isEasyStandard attrs
        then assignHorror iid ElderThing 1
        else assignDamageAndHorror iid ElderThing 1 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          resignedOrDefeatedTrauma attrs
          studentBranch
        Resolution 1 -> do
          record YouDefeatedTheWendigo
          resolution "resolution1"
          eachInvestigator (`sufferMentalTrauma` 1)
          studentBranch
        Resolution 2 -> do
          recordWendigoStillRoams
          resolutionWithXp "resolution2" $ awardScenarioXp attrs NoBonus
          eachInvestigator \iid -> push $ HealTrauma iid 0 1
          epilogue
        Resolution 3 -> do
          recordWendigoStillRoams
          resolutionWithXp "resolution3" $ awardScenarioXp attrs NoBonus
          epilogue
        Resolution 4 -> do
          recordWendigoStillRoams
          resolutionWithXp "resolution4" $ awardScenarioXp attrs $ toBonus "glimpsedTheMythos" 2
          eachInvestigator \iid -> searchCollectionForRandomBasicWeakness iid attrs [Madness]
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

Resolutions 2-4 call this from inside their own @resolutions@ scope, so the
scope is reset rather than nested -- the keys live beside them under @scenario@,
not under @scenario.resolutions@.
-}
epilogue :: ReverseQueue m => m ()
epilogue = scenarioI18n $ scope "epilogue" do
  -- "Do not read the following if all investigators have been driven insane or killed."
  anyLeft <- selectAny $ IncludeEliminated (not_ KilledInvestigator <> not_ InsaneInvestigator)
  when anyLeft do
    -- "Read the following passages each time they match your playing situation."
    flavor $ h "intro" >> p "introBody"

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

    whenHasRecord NormanIsAlive do
      flavor $ h "normanAlive" >> p "normanAliveBody"

    whenHasRecord YouLetNormanDie do
      flavor $ h "normanDead" >> p "normanDeadBody"
      {- "If you added a [tablet] token to the Chaos bag for this game, keep it
      for the rest of your campaign. Otherwise, add a [tablet] token to the
      Chaos bag for the rest of your campaign." Setup's token is only for this
      game, so either branch is one campaign token. -}
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
  -- The guide prints the check itself above the passage, so it is read out too.
  flavor $ p "nadelmannCheck"
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
  push
    $ ScenarioResolution
    $ Resolution
    $ if discoveredAll || discovered == 3 then 2 else if discovered > 0 then 3 else 4

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
awardScenarioXp :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> XpBonus -> m Int
awardScenarioXp attrs extra = do
  savedCharlie <- getHasRecord YouSavedCharlie
  savedProspector <- getHasRecord YouSavedTheGoldProspector
  allGainXpWithBonus' attrs
    $ mconcat
    $ [toBonus "savedCharlie" 1 | savedCharlie]
    <> [toBonus "savedTheGoldProspector" 1 | savedProspector]
    <> [extra]

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
