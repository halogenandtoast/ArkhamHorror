{- | The Scarlet Keys achievement detection.

The detections live on the campaign entity, so the specs drive them with the
same messages the real campaign emits (the Time record count, chaos bag
changes, campaign log records, the epilogue step) rather than replaying whole
scenarios. Reaching 'CampaignStep EpilogueStep' is the campaign's only winning
ending, which is what every "win the campaign" achievement keys on.
-}
module Arkham.Achievements.TheScarletKeysSpec (spec) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Types qualified as Campaign
import Arkham.CampaignLogKey (toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (EpilogueStep, InterludeStep))
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Id (ScarletKeyId (..))
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (EnemyAttrs (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Event.Cards qualified as Events
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Story qualified as Story
import Arkham.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioCountKey (CiviliansSlain))
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Treacheries
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

-- | The message an act pushes as it advances.
advanceActMsg :: CardDef -> Message
advanceActMsg def = AdvanceAct (ActId $ toCardCode def) (TestSource mempty) AdvancedWithOther

{- | Put a story card into play, the way a scenario's setup does, so its clue pool
can be manipulated.
-}
testStoryWithDef :: CardDef -> TestAppT StoryId
testStoryWithDef def = do
  card <- genCard def
  run $ StoryMessage $ Story.PlaceStory card Global
  selectJust $ Matcher.storyIs def

-- | Congress of the Keys' Resolution 1 is the only ending that reaches here.
finishTheCampaign :: TestAppT ()
finishTheCampaign = run $ CampaignStep EpilogueStep

-- | Set the campaign's Time record, the counter every travel step marks up.
timePassed :: Int -> TestAppT ()
timePassed n = run $ RecordCount (toCampaignLogKey Time) n

{- | Put a Scarlet Key into play under the investigator's control and hand back
its id, so its own fast shift ability can be driven.
-}
putScarletKeyIntoPlay :: CardDef -> Investigator -> TestAppT ScarletKeyId
putScarletKeyIntoPlay def self = do
  card <- genCard def
  run $ CreateScarletKeyAt card (AttachedToInvestigator $ toId self)
  pure $ ScarletKeyId (toCardCode card)

{- | Swap the harness scenario without rebuilding the campaign, so achievement
store writes made during an earlier scenario survive into the next one.
'asTheScarletKeysScenario' replaces the campaign too, which would wipe them.
-}
atScenario :: CardCode -> TestAppT ()
atScenario code = do
  scenario' <- testScenario code id
  overTest \g ->
    g
      { gameMode =
          These
            (fromJustNote "campaign attached by asTheScarletKeys" $ modeCampaign (gameMode g))
            scenario'
      }
  tick

{- | Seed the campaign meta the prologue normally builds, with the given travel
history. Every campaign step past the prologue reads this back — including
interlude 37's own handler — so a spec that drives one has to put it there.
-}
withVisited :: [MapLocationId] -> TestAppT ()
withVisited locs = do
  overTest \g ->
    g
      { gameMode =
          first (overAttrs (Campaign.metaL .~ toJSON (initMeta {visitedLocations = locs}))) (gameMode g)
      }
  tick

{- | Replace the campaign chaos bag wholesale. The faces that are not being
tested are irrelevant to detection, so only the watched ones are listed.
-}
withBag :: [ChaosTokenFace] -> TestAppT ()
withBag = run . SetCampaignChaosBag

spec :: Spec
spec = describe "The Scarlet Keys achievements" $ do
  {- Riddles and Rain's treacheries take clues two different ways, and only the
  drop carries a source — see the detection module. 'finishRiddlesAndRain' is the
  'EndOfGame' every one of its resolutions pushes.
  -}
  context "Clued In" $ do
    let inRiddlesAndRain = asTheScarletKeysScenario "09501"
        finishRiddlesAndRain = run $ EndOfGame Nothing

    it "is earned finishing without a treachery taking a clue" . gameTest $ \_ -> do
      inRiddlesAndRain
      earned <- didEarnScarletKeys CluedIn
      finishRiddlesAndRain
      earned `refShouldBe` True

    it "is not earned after dropping a clue to a treachery" . gameTest $ \self -> do
      inRiddlesAndRain
      treachery <- putTreacheryIntoPlay self Treacheries.heavyRain
      earned <- didEarnScarletKeys CluedIn
      run $ InvestigatorPlaceCluesOnLocation (toId self) (toSource treachery) 1
      finishRiddlesAndRain
      earned `refShouldBe` False

    -- Dropping clues to something that is not a treachery is fine: the achievement
    -- is only about treachery cards.
    it "is not disqualified by dropping a clue to a non-treachery" . gameTest $ \self -> do
      inRiddlesAndRain
      earned <- didEarnScarletKeys CluedIn
      run $ InvestigatorPlaceCluesOnLocation (toId self) ScenarioSource 1
      finishRiddlesAndRain
      earned `refShouldBe` True

    -- Both clue-spending treacheries in the game, so neither can be dropped from
    -- 'clueSpendingTreacheries' without a spec going red.
    for_
      [("Pinch in Reality", Treacheries.pinchInReality), ("Hunting Shadow", Treacheries.huntingShadow)]
      \(title, treachery) ->
        it ("is not earned after spending a clue to " <> title)
          . gameTest
          $ \self -> do
            inRiddlesAndRain
            earned <- didEarnScarletKeys CluedIn
            void $ putTreacheryIntoPlay self treachery
            run $ InvestigatorSpendClues (toId self) 1
            finishRiddlesAndRain
            earned `refShouldBe` False

    {- The scenario's own Elder Thing token also makes you spend a clue, and
    'InvestigatorSpendClues' carries no source — so a spend with no clue-spending
    treachery on the table must not disqualify.
    -}
    it "is not disqualified by a spend with no such treachery in play" . gameTest $ \self -> do
      inRiddlesAndRain
      earned <- didEarnScarletKeys CluedIn
      run $ InvestigatorSpendClues (toId self) 1
      finishRiddlesAndRain
      earned `refShouldBe` True

    it "is not earned in a different scenario" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09520"
      earned <- didEarnScarletKeys CluedIn
      finishRiddlesAndRain
      earned `refShouldBe` False

  {- The campaign-level detections. These drive the same campaign-specific messages
  the world map and the interludes emit; 'withVisited' seeds the meta the map keeps
  its travel history in.
  -}
  {- Each city has its own moment; the set is collected in the campaign store, so
  these drive the four other cities' moments and then check that the fifth one
  completes it (and that near-miss variants do not).
  -}
  context "\"I'm Just Here for the Local Cuisine\"" $ do
    -- Marrakesh, Buenos Aires, Tokyo and Kuala Lumpur, leaving Havana outstanding.
    let
      -- Special Delivery ends by offering the world map, so its question is
      -- cleared before driving anything else.
      tokyoHandover = do
        withVisited []
        run $ DoStep 1 $ CampaignStep (InterludeStep 37 Nothing)
        run ClearUI
      buenosAiresAndKualaLumpur self = do
        atScenario "09545"
        run PreScenarioSetup
        run ClearUI
        selangorClub <- testLocationWithDef Locations.selangorClub id
        run $ UseCardAbility (toId self) (toSource selangorClub) 1 [] NoPayment
      restOfTheCities self = tokyoHandover >> buenosAiresAndKualaLumpur self
      fourCities self = do
        asTheScarletKeysScenario "09520"
        run $ DoStep 2 PreScenarioSetup
        restOfTheCities self
      -- A REAL move, not a synthesised EnterLocation: the engine nests
      -- EnterLocation inside Simultaneously/Run, so the two are not equivalent.
      enterCafeLuna self = do
        start <- testLocationWithDef Locations.elMalecon id
        cafe <- testLocationWithDef Locations.cafeLunaCoterieHaunt id
        run $ PlaceInvestigator (toId self) (AtLocation $ toId start)
        moveTo self cafe

    it "is earned entering Cafe Luna with the other four done" . gameTest $ \self -> do
      fourCities self
      earned <- didEarnScarletKeys ImJustHereForTheLocalCuisine
      enterCafeLuna self
      earned `refShouldBe` True

    it "is not earned with Havana still outstanding" . gameTest $ \self -> do
      fourCities self
      earned <- didEarnScarletKeys ImJustHereForTheLocalCuisine
      earned `refShouldBe` False

    {- Dead Heat's intro 4 is the arrive-too-late opening: the cafe is boarded up,
    so Marrakesh is not sampled and the set stays incomplete.
    -}
    it "does not count Dead Heat's intro 4 for Marrakesh" . gameTest $ \self -> do
      asTheScarletKeysScenario "09520"
      run $ DoStep 4 PreScenarioSetup
      restOfTheCities self
      earned <- didEarnScarletKeys ImJustHereForTheLocalCuisine
      enterCafeLuna self
      earned `refShouldBe` False

    -- Branch 3 is the same handover in Lagos, so it is not Tokyo's moment.
    it "does not count Special Delivery 3 for Tokyo" . gameTest $ \self -> do
      asTheScarletKeysScenario "09520"
      run $ DoStep 2 PreScenarioSetup
      withVisited []
      run $ DoStep 3 $ CampaignStep (InterludeStep 37 Nothing)
      run ClearUI
      buenosAiresAndKualaLumpur self
      earned <- didEarnScarletKeys ImJustHereForTheLocalCuisine
      enterCafeLuna self
      earned `refShouldBe` False

  context "With Your Powers Combined…" $ do
    {- Drive the REAL shift, not a synthesised message: a key's own fast ability
    runs @CampaignSpecific "shift[...]"@ on itself via liftRunMessage, so that
    message never reaches the campaign. Both routes end in 'shiftKey', whose
    window is what the detection reads. -}
    let shiftKeyNamed def self = do
          k <- putScarletKeyIntoPlay def self
          run $ UseCardAbility (toId self) (ScarletKeySource k) 1 [] NoPayment
        fourKeys =
          [ Keys.theLastBlossom
          , Keys.theLightOfPharos
          , Keys.theSableGlass
          , Keys.theWeepingLady
          ]

    it "is earned shifting a fifth key in the same turn" . gameTest $ \self -> do
      asTheScarletKeys
      run $ BeginTurn (toId self)
      earned <- didEarnScarletKeys ScarletWithYourPowersCombined
      traverse_ (`shiftKeyNamed` self) fourKeys
      shiftKeyNamed Keys.theEyeOfRavens self
      earned `refShouldBe` True

    it "is not earned shifting only four" . gameTest $ \self -> do
      asTheScarletKeys
      run $ BeginTurn (toId self)
      earned <- didEarnScarletKeys ScarletWithYourPowersCombined
      traverse_ (`shiftKeyNamed` self) fourKeys
      earned `refShouldBe` False

    -- Five shifts of the same key is one key, not five.
    it "is not earned shifting one key five times" . gameTest $ \self -> do
      asTheScarletKeys
      run $ BeginTurn (toId self)
      earned <- didEarnScarletKeys ScarletWithYourPowersCombined
      traverse_ (const $ shiftKeyNamed Keys.theLastBlossom self) [1 :: Int .. 5]
      earned `refShouldBe` False

    it "does not carry keys across a turn boundary" . gameTest $ \self -> do
      asTheScarletKeys
      run $ BeginTurn (toId self)
      earned <- didEarnScarletKeys ScarletWithYourPowersCombined
      traverse_ (`shiftKeyNamed` self) fourKeys
      run $ EndTurn (toId self)
      run $ BeginTurn (toId self)
      shiftKeyNamed Keys.theEyeOfRavens self
      earned `refShouldBe` False

  context "Gift of Gab" $ do
    -- Interlude 37's two intel-handover branches; 2 and 4 are the hand-back.
    let taylorTalks n = run $ DoStep n $ CampaignStep (InterludeStep 37 Nothing)

    it "is earned the third time Taylor says talk" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      earned <- didEarnScarletKeys GiftOfGab
      taylorTalks 1
      taylorTalks 3
      taylorTalks 1
      earned `refShouldBe` True

    it "is not earned after only two" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      earned <- didEarnScarletKeys GiftOfGab
      taylorTalks 1
      taylorTalks 3
      earned `refShouldBe` False

    it "does not count the branches that hand the intel back" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      earned <- didEarnScarletKeys GiftOfGab
      taylorTalks 1
      taylorTalks 2
      taylorTalks 4
      taylorTalks 2
      earned `refShouldBe` False

  context "All Hollow" $ do
    it "is earned unlocking the Bermuda Triangle" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      earned <- didEarnScarletKeys AllHollow
      run $ CampaignSpecific "unlock" (toJSON BermudaTriangle)
      earned `refShouldBe` True

    it "is not earned unlocking anywhere else" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      earned <- didEarnScarletKeys AllHollow
      run $ CampaignSpecific "unlock" (toJSON KualaLumpur)
      earned `refShouldBe` False

  context "Take That, Ghulat" $ do
    it "is earned finishing Dead Heat with no civilian slain" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09520"
      earned <- didEarnScarletKeys TakeThatGhulat
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    -- slayCivilian is the only writer of the count, so any value above zero is a
    -- civilian actually lost.
    it "is not earned after a civilian is slain" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09520"
      earned <- didEarnScarletKeys TakeThatGhulat
      run $ ScenarioCountIncrementBy CiviliansSlain 1
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  context "What's in a Name?" $ do
    it "is earned on Dead Heat's Resolution 3 record" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys WhatsInAName
      run $ Record $ toCampaignLogKey AmaranthHasLeftTheCoterie
      earned `refShouldBe` True

    it "is not earned by another Amaranth outcome" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys WhatsInAName
      run $ Record $ toCampaignLogKey YouHaventSeenTheLastOfAmaranth
      earned `refShouldBe` False

  context "More Like \"Destroyed\" Chimera" $ do
    let forms =
          [ Enemies.voidChimeraTrueForm
          , Enemies.voidChimeraFellbeak
          , Enemies.voidChimeraEarsplitter
          , Enemies.voidChimeraGorefeaster
          , Enemies.voidChimeraFellhound
          ]
        defeatForm def = do
          e <- testEnemyWithDef def id
          run $ Defeated (toTarget e) (toCardId e) (TestSource mempty) []

    it "is earned defeating all five forms" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09609"
      earned <- didEarnScarletKeys MoreLikeDestroyedChimera
      traverse_ defeatForm forms
      earned `refShouldBe` True

    it "is not earned with a form still standing" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09609"
      earned <- didEarnScarletKeys MoreLikeDestroyedChimera
      traverse_ defeatForm (drop 1 forms)
      earned `refShouldBe` False

    -- Defeating the same form five times is one form, not five.
    it "is not earned defeating one form five times" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09609"
      earned <- didEarnScarletKeys MoreLikeDestroyedChimera
      traverse_ (const $ defeatForm Enemies.voidChimeraTrueForm) [1 :: Int .. 5]
      earned `refShouldBe` False

  context "Who Watches the Watcher?" $ do
    it "is earned when Seeing Red becomes the agenda deck" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09545"
      earned <- didEarnScarletKeys WhoWatchesTheWatcher
      card <- genCard Agendas.seeingRed
      run $ SetCurrentAgendaDeck 1 [card]
      earned `refShouldBe` True

    it "is not earned by the ordinary agenda" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09545"
      earned <- didEarnScarletKeys WhoWatchesTheWatcher
      card <- genCard Agendas.whereIsShe
      run $ SetCurrentAgendaDeck 1 [card]
      earned `refShouldBe` False

  context "Under My Umbrella" $ do
    let tuckGeists self = do
          tzu <- testEnemyWithDef Enemies.tzuSanNiangTheLadyWithTheRedParasol id
          geist <- genCard Enemies.uncannyShadowPlayfulShadows
          run $ PlaceUnderneath (toTarget tzu) [geist]
          pure self

    it "is earned finishing with no Geist devoured" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09660"
      earned <- didEarnScarletKeys UnderMyUmbrella
      run $ EndOfGame Nothing
      earned `refShouldBe` True

    it "is not earned after Tzu San Niang devours one" . gameTest $ \self -> do
      asTheScarletKeysScenario "09660"
      earned <- didEarnScarletKeys UnderMyUmbrella
      void $ tuckGeists self
      run $ EndOfGame Nothing
      earned `refShouldBe` False

  {- "Lost and Found": the Twisted Antiprism handed to an investigator while Clues
  Unveiled is bare. The Unveiling's clue pool is seeded by Dealings in the Dark's
  own setup from the campaign's Time, so these drive the real scenario rather than
  the harness shell, and vary Time to move the clue count.
  -}
  context "Lost and Found" $ do
    let takeAntiprism self = do
          card <- genCard Keys.theTwistedAntiprism
          run $ CreateScarletKeyAt card (AttachedToInvestigator $ toId self)

    it "is earned taking the Antiprism with Clues Unveiled bare" . gameTest $ \self -> do
      asTheScarletKeysScenario "09566"
      earned <- didEarnScarletKeys LostAndFound
      takeAntiprism self
      earned `refShouldBe` True

    {- The Unveiling reads the current act step in its modifiers, so it needs an act
    on the table before it can be put into play at all.
    -}
    it "is not earned with a clue still on Clues Unveiled" . gameTest $ \self -> do
      asTheScarletKeysScenario "09566"
      act <- genCard Acts.searchForTheTalisman
      run $ SetActDeckCards 1 [act]
      run SetActDeck
      unveiling <- testStoryWithDef Stories.theUnveiling
      run $ PlaceTokens (TestSource mempty) (toTarget unveiling) Token.Clue 1
      earned <- didEarnScarletKeys LostAndFound
      takeAntiprism self
      earned `refShouldBe` False

    it "is not earned in a different scenario" . gameTest $ \self -> do
      asTheScarletKeysScenario "09591"
      earned <- didEarnScarletKeys LostAndFound
      takeAntiprism self
      earned `refShouldBe` False

  context "I Like Tower Defense Games" $ do
    let advanceRabbits = run $ advanceActMsg Acts.rabbitsWhoRunV1
        destroyLocus self def = do
          locus <- testAssetWithDef def id self
          run $ AssetDefeated (TestSource mempty) (toId locus)

    it "is earned advancing v. I with every Key Locus standing" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09635"
      earned <- didEarnScarletKeys ILikeTowerDefenseGames
      advanceRabbits
      earned `refShouldBe` True

    for_
      [ ("Last Bastion", Assets.keyLocusLastBastion)
      , ("Defensive Barrier", Assets.keyLocusDefensiveBarrier)
      ]
      \(title, def) ->
        it ("is not earned after the " <> title <> " is destroyed") . gameTest $ \self -> do
          asTheScarletKeysScenario "09635"
          earned <- didEarnScarletKeys ILikeTowerDefenseGames
          destroyLocus self def
          advanceRabbits
          earned `refShouldBe` False

  {- "Play With Your Food": steal The Light of Pharos off a bearer sitting on
  exactly 1 health. The steal is the key being re-placed onto an investigator.
  -}
  context "Play With Your Food" $ do
    let bearerOn health def self = do
          enemy <- testEnemyWithDef def (\a -> a {enemyHealthDamage = 0})
          card <- genCard Keys.theLightOfPharos
          run $ CreateScarletKeyAt card (AttachedToEnemy $ toId enemy)
          damage <- subtract health . fromJustNote "enemy has health" <$> field Enemy.EnemyHealth (toId enemy)
          when (damage > 0) $ run $ PlaceTokens (TestSource mempty) (toTarget enemy) Token.Damage damage
          pure (ScarletKeyId $ toCardCode card, self)

    for_
      [ ("The Claret Knight", Enemies.theClaretKnightCoterieKingpin)
      , ("The Beast in a Cowl of Crimson", Enemies.theBeastInACowlOfCrimsonWolfInSheepsClothing)
      ]
      \(title, def) ->
        it ("is earned stealing it from " <> title <> " on 1 health") . gameTest $ \self -> do
          asTheScarletKeysScenario "09635"
          (kid, _) <- bearerOn 1 def self
          earned <- didEarnScarletKeys PlayWithYourFood
          run $ PlaceScarletKey kid (AttachedToInvestigator $ toId self)
          earned `refShouldBe` True

    it "is not earned stealing it from a bearer on 2 health" . gameTest $ \self -> do
      asTheScarletKeysScenario "09635"
      (kid, _) <- bearerOn 2 Enemies.theClaretKnightCoterieKingpin self
      earned <- didEarnScarletKeys PlayWithYourFood
      run $ PlaceScarletKey kid (AttachedToInvestigator $ toId self)
      earned `refShouldBe` False

  {- "Porque No Los Dos?": both Desis to the same damage assignment. Simultaneous
  defeats are queued together, so the second copy is already out of health when the
  first Defeated dispatches; a separate later blow is not.
  -}
  context "Porque No Los Dos?" $ do
    let bothDesis = do
          a <- testEnemyWithDef Enemies.desiderioDelgadoAlvarez106 id
          b <- testEnemyWithDef Enemies.desiderioDelgadoAlvarez107 id
          pure (a, b)
        defeatMsg e = Defeated (toTarget e) (toCardId e) (TestSource mempty) []

    -- One effect killing both queues both Defeated messages before either runs.
    it "is earned when both are defeated by the same effect" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09591"
      (a, b) <- bothDesis
      earned <- didEarnScarletKeys PorqueNoLosDos
      pushAndRunAll [defeatMsg a, defeatMsg b]
      earned `refShouldBe` True

    {- The real thing: one Dynamite Blast at a location holding both copies. This is
    the flow the fixture game hands over, so it is driven end to end rather than
    assumed.
    -}
    it "is earned killing both with one Dynamite Blast" . gameTest $ \self -> do
      asTheScarletKeysScenario "09591"
      (a, b) <- bothDesis
      location <- testLocation
      a `spawnAt` location
      b `spawnAt` location
      self `moveTo` location
      -- Health is 2 + 2 per player, so soften both to within the blast's 3 damage.
      for_ [a, b] \e -> run $ PlaceDamage (TestSource mempty) (toTarget e) 1
      earned <- didEarnScarletKeys PorqueNoLosDos
      self `putCardIntoPlay` Events.dynamiteBlast
      click "choose your location"
      applyAllDamage
      earned `refShouldBe` True

    -- Two separate blows: the first Defeated is consumed before the second exists.
    it "is not earned defeating them one at a time" . gameTest $ \_ -> do
      asTheScarletKeysScenario "09591"
      (a, b) <- bothDesis
      earned <- didEarnScarletKeys PorqueNoLosDos
      run $ defeatMsg a
      run $ defeatMsg b
      earned `refShouldBe` False

  {- "Key to My Heart" reports checklist progress as each key is collected, which
  the campaign signals with 'setBearer'.
  -}
  context "Key to My Heart" $ do
    let collect def status = run $ CampaignSpecific "setBearer" (toJSON (toCardCode def, status))
        mine = KeyWithInvestigator "01001"

    it "reports a key as it is collected" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      progressed <- didProgressScarletKeys KeyToMyHeart ["TheEyeOfRavens"]
      collect Keys.theEyeOfRavens mine
      progressed `refShouldBe` True

    -- The set accumulates across the campaign, in printed checklist order.
    it "accumulates the keys collected so far" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      progressed <- didProgressScarletKeys KeyToMyHeart ["TheLastBlossom", "TheEyeOfRavens"]
      collect Keys.theEyeOfRavens mine
      collect Keys.theLastBlossom mine
      progressed `refShouldBe` True

    -- A key that ends up with a Coterie member is not collected.
    it "does not report a key held by an enemy" . gameTest $ \_ -> do
      asTheScarletKeys
      withVisited []
      progressed <- didProgressScarletKeys KeyToMyHeart ["TheEyeOfRavens"]
      collect Keys.theEyeOfRavens (KeyWithEnemy "09606" Nothing)
      progressed `refShouldBe` False

  context "Speed Demon" $ do
    it "is earned winning with exactly 17 time passed" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys SpeedDemon
      timePassed 17
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned winning with 18 time passed" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys SpeedDemon
      timePassed 18
      finishTheCampaign
      earned `refShouldBe` False

  context "Trust Nobody" $ do
    it "is earned with four Elder Things and none ever removed" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustNobody
      withBag [ElderThing, ElderThing, ElderThing, ElderThing]
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned with only three Elder Things" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustNobody
      withBag [ElderThing, ElderThing, ElderThing, Tablet]
      finishTheCampaign
      earned `refShouldBe` False

    -- Swapping an Elder Thing away for a Tablet is the "trust" choice; that the
    -- bag is topped back up to four later does not undo it.
    it "is not earned after ever removing an Elder Thing" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustNobody
      withBag [ElderThing, Tablet]
      run $ RemoveChaosToken ElderThing
      withBag [ElderThing, ElderThing, ElderThing, ElderThing]
      finishTheCampaign
      earned `refShouldBe` False

    -- 'swapTokens' pushes its removal unconditionally, so a face already down to
    -- zero keeps producing no-op removals; those are not removals.
    it "is not disqualified by a removal with none in the bag" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustNobody
      withBag [Tablet]
      run $ RemoveChaosToken ElderThing
      withBag [ElderThing, ElderThing, ElderThing, ElderThing]
      finishTheCampaign
      earned `refShouldBe` True

  context "Trust Everybody" $ do
    it "is earned with four Tablets and none ever removed" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustEverybody
      withBag [Tablet, Tablet, Tablet, Tablet]
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned with only three Tablets" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustEverybody
      withBag [Tablet, Tablet, Tablet, ElderThing]
      finishTheCampaign
      earned `refShouldBe` False

    it "is not earned after ever removing a Tablet" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustEverybody
      withBag [ElderThing, Tablet]
      run $ RemoveChaosToken Tablet
      withBag [Tablet, Tablet, Tablet, Tablet]
      finishTheCampaign
      earned `refShouldBe` False

    -- The two are independent: trading Tablets away for Elder Things breaks
    -- Trust Everybody without touching Trust Nobody.
    it "is not disqualified by removing an Elder Thing" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys TrustEverybody
      withBag [ElderThing, Tablet]
      run $ RemoveChaosToken ElderThing
      withBag [Tablet, Tablet, Tablet, Tablet]
      finishTheCampaign
      earned `refShouldBe` True

  {- Congress of the Keys' trial offers the two Coterie-siding outcomes only when
  the vote goes the cell's way; each branch writes its own record, which is what
  the detection keys on. The specs drive the record directly — the trial itself is
  a long chain of setup flavor text with no bearing on the earn.
  -}
  context "Red Looks Good on Me" $ do
    it "is earned joining the Red Coterie" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys RedLooksGoodOnMe
      run $ Record $ toCampaignLogKey TheCellJoinedTheRedCoterie
      earned `refShouldBe` True

    it "is not earned when the Coterie merely spares the cell" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys RedLooksGoodOnMe
      run $ Record $ toCampaignLogKey TheRedCoterieSparedTheCell
      earned `refShouldBe` False

  context "Bloody Red Revolution" $ do
    it "is earned overthrowing the Red Coterie" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys BloodyRedRevolution
      run $ Record $ toCampaignLogKey TheCellOverthrewTheRedCoterie
      earned `refShouldBe` True

    -- The two outcomes are exclusive branches of the same choice.
    it "is not earned joining the Red Coterie instead" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys BloodyRedRevolution
      run $ Record $ toCampaignLogKey TheCellJoinedTheRedCoterie
      earned `refShouldBe` False

  context "Here is Your Badge" $ do
    it "is earned when the epilogue grants a permanent position" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys HereIsYourBadge
      run $ Record $ toCampaignLogKey TheCellWasGivenAPermanentPosition
      earned `refShouldBe` True

    it "is not earned when the cell is dismantled instead" . gameTest $ \_ -> do
      asTheScarletKeys
      earned <- didEarnScarletKeys HereIsYourBadge
      run $ Record $ toCampaignLogKey TheCellWasDismantled
      finishTheCampaign
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Line in the Sand with three active ultimatums" . gameTest $ \_ -> do
      asTheScarletKeys
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnScarletKeys ScarletLineInTheSand
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Line in the Sand with only two" . gameTest $ \_ -> do
      asTheScarletKeys
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnScarletKeys ScarletLineInTheSand
      finishTheCampaign
      earned `refShouldBe` False

    it "earns Global Expertise on Expert" . gameTest $ \_ -> do
      asTheScarletKeysWith Expert
      earned <- didEarnScarletKeys GlobalExpertise
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Global Expertise below Expert" . gameTest $ \_ -> do
      asTheScarletKeysWith Hard
      earned <- didEarnScarletKeys GlobalExpertise
      finishTheCampaign
      earned `refShouldBe` False
