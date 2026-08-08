{- | Edge of the Earth achievement detection.

The detections live on the campaign entity, so most specs drive them with the
same messages the real scenarios and interludes emit (camp records, victory
display additions, key placements, 'EndOfGame') rather than replaying whole
scenarios. Scenario-scoped detections use 'asEdgeOfTheEarthScenario', which
swaps in a Gathering-shaped shell carrying the real scenario id — the scenario's
own behaviour never runs, which is exactly why the detections key on messages
rather than on resolutions.
-}
module Arkham.Achievements.EdgeOfTheEarthSpec (spec) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Campaign.Types qualified as Campaign
import Arkham.CampaignLog (
  CampaignLogPartner (..),
  PartnerStatus (Resolute, Safe),
  partnersL,
 )
import Arkham.CampaignLogKey (recorded, toCampaignLogKey)
import Arkham.CampaignStep (CampaignStep (EpilogueStep))
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Campaigns.EdgeOfTheEarth.Partner (expeditionTeam)
import Arkham.Campaigns.EdgeOfTheEarth.Seal (Seal (..), SealKind (..))
import Arkham.Difficulty
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exhaust (mkExhaustion)
import Arkham.Game.Settings (settingsAchievementsEnabled)
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (revealedL)
import Arkham.Movement (move)
import Arkham.Placement
import Arkham.Scenario.Types qualified as Scenario
import Arkham.Source
import Arkham.Story.Cards qualified as Stories
import Arkham.Token qualified as Token
import Arkham.Treachery.Cards qualified as Treacheries
import Helpers.Achievements
import Helpers.UltimatumsAndBoons (Ultimatum (..), withUltimatums)
import TestImport.New

{- | Seed the expedition team into the campaign log, the way StartCampaign does.
The harness attaches the campaign without running StartCampaign, and Edge of the
Earth's epilogue reads every partner's status, so without this it throws before
any detection can be observed.
-}
seedPartners :: Campaign.CampaignAttrs -> Campaign.CampaignAttrs
seedPartners attrs =
  attrs
    & Campaign.logL
    . partnersL
    %~ \ps -> foldl' addMissing ps (toList expeditionTeam)
 where
  -- Only fill gaps: a spec that set a partner Resolute must keep that status.
  addMissing m def = insertWith (\_ old -> old) (toCardCode def) (CampaignLogPartner 0 0 Safe) m

-- | Put exactly @n@ Frost tokens in the campaign chaos bag the epilogue reads.
withFrostTokens :: Int -> TestAppT ()
withFrostTokens n = do
  let seed attrs =
        attrs
          { Campaign.campaignChaosBag =
              replicate n FrostToken <> filter (/= FrostToken) (Campaign.campaignChaosBag attrs)
          }
  overTest \g -> g {gameMode = first (overAttrs seed) (gameMode g)}
  tick

-- | Every surviving ending of the campaign routes through the epilogue.
finishTheCampaign :: TestAppT ()
finishTheCampaign = do
  overTest \g -> g {gameMode = first (overAttrs seedPartners) (gameMode g)}
  run $ CampaignStep EpilogueStep

-- | testAssetWithDef leaves an asset Unplaced with no controller.
controlledBy :: Investigator -> Asset.AssetAttrs -> Asset.AssetAttrs
controlledBy i = (Asset.controllerL ?~ toId i) . (Asset.placementL .~ InPlayArea (toId i))

{- | An asset's ability source, the shape the engine actually produces: an
'AssetSource' wrapped in 'AbilitySource'. Sourcing by card code (as an earlier
version of these specs did) never occurs in a real game and hid three broken
detections.
-}
abilityOf :: Asset.Asset -> Int -> Source
abilityOf asset n = AbilitySource (AssetSource $ toId asset) n

spec :: Spec
spec = describe "Edge of the Earth achievements" $ do
  context "Safe Bet" $ do
    -- Camping is recording one of the Camp_* keys at Ice and Death, Part I.
    let campAt key = run $ Record (toCampaignLogKey key)

    it "is earned camping at a shelter value of 8" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08501a"
      earned <- didEarnEdgeOfTheEarth SafeBet
      campAt Camp_CrystallineCavern
      earned `refShouldBe` True

    it "is not earned camping somewhere less sheltered" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08501a"
      earned <- didEarnEdgeOfTheEarth SafeBet
      campAt Camp_BarrierCamp
      earned `refShouldBe` False

    it "is not earned while achievements are disabled" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08501a"
      overTest \g -> g {gameSettings = (gameSettings g) {settingsAchievementsEnabled = False}}
      earned <- didEarnEdgeOfTheEarth SafeBet
      campAt Camp_CrystallineCavern
      earned `refShouldBe` False

  context "Wuk Wuk Boom" $ do
    -- UseThisAbility is a match-only pattern; the real message is UseCardAbility.
    let blast self dynamite =
          run $ UseCardAbility (toId self) (abilityOf dynamite 1) 1 [] NoPayment
        defeatPenguin dynamite = do
          location <- testLocation
          penguin <- testEnemyWithDef Enemies.giantAlbinoPenguin id
          penguin `spawnAt` location
          run $ Defeated (toTarget penguin) (toCardId penguin) (abilityOf dynamite 1) []
        withDynamite self = testAssetWithDef Assets.dynamite (controlledBy self) self

    it "is earned when one blast kills two penguins" . gameTest $ \self -> do
      asEdgeOfTheEarth
      dynamite <- withDynamite self
      earned <- didEarnEdgeOfTheEarth WukWukBoom
      blast self dynamite
      defeatPenguin dynamite
      earned `refShouldBe` False
      defeatPenguin dynamite
      earned `refShouldBe` True

    -- The counter resets as the ability is used, so kills from separate blasts
    -- must not add up.
    it "is not earned by two separate blasts" . gameTest $ \self -> do
      asEdgeOfTheEarth
      dynamite <- withDynamite self
      earned <- didEarnEdgeOfTheEarth WukWukBoom
      blast self dynamite
      defeatPenguin dynamite
      blast self dynamite
      defeatPenguin dynamite
      earned `refShouldBe` False

    it "does not count penguins killed by anything else" . gameTest $ \self -> do
      asEdgeOfTheEarth
      dynamite <- withDynamite self
      earned <- didEarnEdgeOfTheEarth WukWukBoom
      blast self dynamite
      location <- testLocation
      replicateM_ 2 do
        penguin <- testEnemyWithDef Enemies.giantAlbinoPenguin id
        penguin `spawnAt` location
        run $ Defeated (toTarget penguin) (toCardId penguin) (TestSource mempty) []
      earned `refShouldBe` False

  context "Chaos Chaos" $ do
    {- Collecting is a key moving onto an investigator; spending is one moving back
    onto a location while an investigator still holds it. Both halves need ten, so
    the same key is cycled rather than needing ten distinct colours.
    -}
    let collect i k = run $ PlaceKey (toTarget i) k
        -- Paying a key cost pushes PlaceKey ScenarioTarget, not a location.
        spend _ k = run $ PlaceKey ScenarioTarget k
        cycleKey i l k = collect i k >> spend l k

    it "is earned once ten are collected and ten are spent" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08621"
      location <- testLocation
      earned <- didEarnEdgeOfTheEarth ChaosChaos
      replicateM_ 9 $ cycleKey self location RedKey
      earned `refShouldBe` False
      cycleKey self location RedKey
      earned `refShouldBe` True

    it "is not earned by collecting ten without spending them" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08621"
      earned <- didEarnEdgeOfTheEarth ChaosChaos
      replicateM_ 10 $ collect self RedKey
      earned `refShouldBe` False

    -- Spending only counts while an investigator actually holds the key, so
    -- shuffling keys between locations must not inflate the tally.
    -- A key that was never collected is not a spend, however it moves.
    it "is not earned by moving uncollected keys around" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08621"
      earned <- didEarnEdgeOfTheEarth ChaosChaos
      replicateM_ 20 $ run $ PlaceKey ScenarioTarget RedKey
      earned `refShouldBe` False

    it "does not count keys handled in another scenario" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08501a"
      location <- testLocation
      earned <- didEarnEdgeOfTheEarth ChaosChaos
      replicateM_ 10 $ cycleKey self location RedKey
      earned `refShouldBe` False

  context "The Sound of Madness" $ do
    {- Counted off the cards drawn, not the deck: most Tekeli-li are shuffled into a
    player deck and arrive through an ordinary draw.
    -}
    let drawTreachery self def = do
          card <- genCard def
          run $ DrewTreachery (toId self) Nothing card
        drawTekelili self n = replicateM_ n (drawTreachery self Treacheries.tekelili_223)
        drawOther self n = replicateM_ n (drawTreachery self Treacheries.huntingShadow)

    it "is earned on the tenth Tekeli-li drawn" . gameTest $ \self -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth TheSoundOfMadness
      drawTekelili self 9
      earned `refShouldBe` False
      drawTekelili self 1
      earned `refShouldBe` True

    it "is not earned after nine" . gameTest $ \self -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth TheSoundOfMadness
      drawTekelili self 9
      earned `refShouldBe` False

    it "does not count other cards drawn" . gameTest $ \self -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth TheSoundOfMadness
      drawOther self 10
      earned `refShouldBe` False

  context "No Respect For the Dead" $ do
    let fifth = Assets.kenslersLog
        memorials =
          [ Assets.claypoolsFurs
          , Assets.collectedWorksOfPoe
          , Assets.cookiesCustom32
          , Assets.ellsworthsBoots
          ]

    it "is earned controlling five Memorials assets" . gameTest $ \self -> do
      asEdgeOfTheEarth
      for_ memorials \def -> void $ testAssetWithDef def (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth NoRespectForTheDead
      _ <- testAssetWithDef fifth (controlledBy self) self
      run . CardEnteredPlay (toId self) =<< genCard fifth
      earned `refShouldBe` True

    it "is not earned with only four" . gameTest $ \self -> do
      asEdgeOfTheEarth
      for_ memorials \def -> void $ testAssetWithDef def (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth NoRespectForTheDead
      run . CardEnteredPlay (toId self) =<< genCard Assets.claypoolsFurs
      earned `refShouldBe` False

  context "There and Back Again" $ do
    let survive defs =
          run
            $ RecordSetInsert (toCampaignLogKey TheSurvivorsOfTheExpeditionWere)
            $ map (recorded . toCardCode) defs

    it "reports only the expedition members who came home" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      survive [Assets.drAmyKenslerProfessorOfBiology, Assets.danforthBrilliantStudent]
      progressed <- didProgressEdgeOfTheEarth ThereAndBackAgain ["DrAmyKensler", "Danforth"]
      finishTheCampaign
      progressed `refShouldBe` True

    it "reports nothing when nobody survived" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      progressed <- didProgressEdgeOfTheEarth ThereAndBackAgain ["DrAmyKensler"]
      finishTheCampaign
      progressed `refShouldBe` False

  context "Abandoned and Alone" $ do
    -- Taking a partner is the scenario handing an investigator that partner's code.
    let takeAlong self def =
          run $ HandleTargetChoice (toId self) ScenarioSource (CardCodeTarget $ toCardCode def)

    it "is earned finishing the campaign having never taken a partner" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth AbandonedAndAlone
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned once a partner has come along" . gameTest $ \self -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth AbandonedAndAlone
      takeAlong self Assets.drMalaSinhaDaringPhysician
      finishTheCampaign
      earned `refShouldBe` False

  context "Look at All This Stuff!" $ do
    {- It fires on arrival at The Summit, not at the end of the scenario, so the
    spec walks an investigator onto the summit carrying the supplies.
    -}
    let supplies =
          [ Assets.greenSoapstoneJinxedIdol
          , Assets.woodenSledge
          , Assets.dynamite
          , Assets.miasmicCrystalStrangeEvidence
          , Assets.mineralSpecimen
          , Assets.smallRadio
          , Assets.spareParts
          ]
        carry self defs =
          for_ defs \def -> void $ testAssetWithDef def (controlledBy self) self
        climb self = do
          summit <- testLocationWithDef Locations.theSummit id
          run . MoveTo =<< move (TestSource mempty) (toId self) (toId summit)

    it "is earned reaching The Summit carrying all seven supplies" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08596"
      carry self supplies
      earned <- didEarnEdgeOfTheEarth LookAtAllThisStuff
      climb self
      earned `refShouldBe` True

    it "is not earned having left one behind" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08596"
      carry self (drop 1 supplies)
      earned <- didEarnEdgeOfTheEarth LookAtAllThisStuff
      climb self
      earned `refShouldBe` False

    -- Carrying the full set somewhere that is not the summit is not enough.
    it "is not earned moving anywhere else" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08596"
      carry self supplies
      earned <- didEarnEdgeOfTheEarth LookAtAllThisStuff
      elsewhere <- testLocation
      run . MoveTo =<< move (TestSource mempty) (toId self) (toId elsewhere)
      earned `refShouldBe` False

  context "In Your Head" $ do
    {- It waits for the scenario to finish - Shadow of the Past advancing - rather
    than firing as the ninth memory is banished.
    -}
    let seedVictory defs = do
          cards <- traverse genCard defs
          let seed attrs = attrs {Scenario.scenarioVictoryDisplay = cards}
          overTest \g -> g {gameMode = second (overAttrs seed) (gameMode g)}
          tick
        finishMirage =
          run
            $ AdvanceAct
              (ActId $ toCardCode Acts.shadowOfThePastV1)
              (TestSource mempty)
              AdvancedWithOther
        banish def = do
          card <- genCard def
          run $ StoryMessage $ PlaceStory card Global
          run $ AddToVictory Nothing (StoryTarget $ StoryId $ toCardCode def)
        memories =
          [ Stories.memoryOfAHuntGoneAwry
          , Stories.memoryOfALostPatient
          , Stories.memoryOfAMissingFather
          , Stories.memoryOfARavagedCountry
          , Stories.memoryOfARegretfulVoyage
          , Stories.memoryOfAnUnspeakableEvil
          , Stories.memoryOfATerribleDiscovery
          , Stories.memoryOfAnAlienTranslation
          , Stories.memoryOfAnUnrequitedLove
          ]

    it "is earned finishing with nine story cards banished" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08549"
      seedVictory memories
      earned <- didEarnEdgeOfTheEarth InYourHead
      finishMirage
      earned `refShouldBe` True

    it "is not earned finishing with eight" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08549"
      seedVictory (take 8 memories)
      earned <- didEarnEdgeOfTheEarth InYourHead
      finishMirage
      earned `refShouldBe` False

    -- Banishing the ninth is not enough on its own; the scenario has to end.
    it "is not earned merely by banishing the ninth" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08549"
      seedVictory (take 8 memories)
      earned <- didEarnEdgeOfTheEarth InYourHead
      banish (memories !!? 8 & fromJustNote "ninth memory")
      earned `refShouldBe` False

  context "Knock, Knock" $ do
    -- It fires as the last seal goes down, not at the end of the scenario.
    let placeSeal l kind active = run $ PlaceSeal (toTarget l) (Seal kind active Nothing)
        allButLast = filter (/= SealE) [minBound @SealKind ..]

    it "is earned placing the fifth active seal" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648a"
      location <- testLocation
      for_ allButLast \kind -> placeSeal location kind True
      earned <- didEarnEdgeOfTheEarth KnockKnock
      placeSeal location SealE True
      earned `refShouldBe` True

    it "is not earned on the fourth" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648a"
      location <- testLocation
      earned <- didEarnEdgeOfTheEarth KnockKnock
      for_ allButLast \kind -> placeSeal location kind True
      earned `refShouldBe` False

    -- Placed but never activated does not count.
    it "is not earned when the last seal is inactive" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648a"
      location <- testLocation
      for_ allButLast \kind -> placeSeal location kind True
      earned <- didEarnEdgeOfTheEarth KnockKnock
      placeSeal location SealE False
      earned `refShouldBe` False

    -- A seal an investigator is still carrying has not been placed.
    it "is not earned when the last seal is still being carried" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08648a"
      location <- testLocation
      for_ allButLast \kind -> placeSeal location kind True
      earned <- didEarnEdgeOfTheEarth KnockKnock
      placeSeal self SealE True
      earned `refShouldBe` False

  context "Mad With Power" $ do
    let exhaustMadness n = do
          location <- testLocation
          replicateM_ n do
            madness <- testEnemyWithDef Enemies.theNamelessMadness id
            madness `spawnAt` location
            run $ Exhaust $ mkExhaustion (TestSource mempty) madness

    it "is earned on the fifteenth exhausted" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648b"
      earned <- didEarnEdgeOfTheEarth MadWithPower
      exhaustMadness 14
      earned `refShouldBe` False
      exhaustMadness 1
      earned `refShouldBe` True

    it "is not earned after fourteen" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648b"
      earned <- didEarnEdgeOfTheEarth MadWithPower
      exhaustMadness 14
      earned `refShouldBe` False

  context "Construct Additional Pylons" $ do
    let advancePylonAct =
          run $ AdvanceAct (ActId $ toCardCode Acts.collapseThePylons) (TestSource mempty) AdvancedWithOther
        collapse n = replicateM_ n do
          pylon <- testLocationWithDef Locations.mistPylon_174 (revealedL .~ True)
          run $ PlaceTokens ScenarioSource (toTarget pylon) Token.Damage 4

    it "is earned collapsing all five and escaping" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648b"
      collapse 5
      advancePylonAct
      earned <- didEarnEdgeOfTheEarth ConstructAdditionalPylons
      pushEnd (EndOfGame Nothing) >> runMessages
      earned `refShouldBe` True

    it "is not earned with only four collapsed" . gameTest $ \_ -> do
      asEdgeOfTheEarthScenario "08648b"
      collapse 4
      advancePylonAct
      earned <- didEarnEdgeOfTheEarth ConstructAdditionalPylons
      pushEnd (EndOfGame Nothing) >> runMessages
      earned `refShouldBe` False

  context "Sorry, I'm All Out of Dog Puns" $ do
    it "is earned with Anyu and four other Dogs" . gameTest $ \self -> do
      asEdgeOfTheEarth
      _ <- testAssetWithDef Assets.anyuFaithfulCompanion (controlledBy self) self
      replicateM_ 4 $ void $ testAssetWithDef Assets.sledDog (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth SorryImAllOutOfDogPuns
      run . CardEnteredPlay (toId self) =<< genCard Assets.sledDog
      earned `refShouldBe` True

    it "is not earned with only three other Dogs" . gameTest $ \self -> do
      asEdgeOfTheEarth
      _ <- testAssetWithDef Assets.anyuFaithfulCompanion (controlledBy self) self
      replicateM_ 3 $ void $ testAssetWithDef Assets.sledDog (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth SorryImAllOutOfDogPuns
      run . CardEnteredPlay (toId self) =<< genCard Assets.sledDog
      earned `refShouldBe` False

    it "is not earned without Anyu" . gameTest $ \self -> do
      asEdgeOfTheEarth
      replicateM_ 4 $ void $ testAssetWithDef Assets.sledDog (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth SorryImAllOutOfDogPuns
      run . CardEnteredPlay (toId self) =<< genCard Assets.sledDog
      earned `refShouldBe` False

  context "This Was Your Idea" $ do
    let healDanforth self source n = do
          danforth <- testAssetWithDef Assets.danforthBrilliantStudent (controlledBy self) self
          run $ HealHorror (toTarget danforth) source n

    it "is earned on the fourth horror healed" . gameTest $ \self -> do
      asEdgeOfTheEarth
      dyer <- testAssetWithDef Assets.professorWilliamDyerProfessorOfGeology (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth ThisWasYourIdea
      healDanforth self (abilityOf dyer 1) 2
      earned `refShouldBe` False
      healDanforth self (abilityOf dyer 1) 2
      earned `refShouldBe` True

    it "is not earned after two" . gameTest $ \self -> do
      asEdgeOfTheEarth
      dyer <- testAssetWithDef Assets.professorWilliamDyerProfessorOfGeology (controlledBy self) self
      earned <- didEarnEdgeOfTheEarth ThisWasYourIdea
      healDanforth self (abilityOf dyer 1) 2
      earned `refShouldBe` False

    it "does not count healing from anyone else" . gameTest $ \self -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth ThisWasYourIdea
      healDanforth self (TestSource mempty) 4
      earned `refShouldBe` False

  context "Kind of a Hat on a Hat" $ do
    {- The chain is strict: the Sledge must be played out of a Backpack and the very
    next ACTION must be its own ability, attaching another Backpack.
    -}
    let stage self = do
          bpack <- testAssetWithDef Assets.backpack (controlledBy self) self
          sledgeCard <- genCard Assets.woodenSledge
          run $ PlaceUnderneath (toTarget bpack) [sledgeCard]
          sledge <- testAssetWithDef Assets.woodenSledge (controlledBy self) self
          pure (sledgeCard, sledge)
        playSledge self card = run $ InitiatePlayCard (toId self) card Nothing NoPayment [] False
        arrive self card = run $ CardEnteredPlay (toId self) card
        -- ActiveCost pushes this as each action completes.
        finishAction self = run $ TakenActions (toId self) [#play]
        attachBackpack sledge = do
          backpackCard <- genCard Assets.backpack
          run $ PlaceUnderneath (toTarget sledge) [backpackCard]

    it "is earned playing the Sledge from a Backpack then attaching one" . gameTest $ \self -> do
      asEdgeOfTheEarth
      (card, sledge) <- stage self
      earned <- didEarnEdgeOfTheEarth KindOfAHatOnAHat
      playSledge self card
      arrive self card
      finishAction self
      attachBackpack sledge
      earned `refShouldBe` True

    it "is not earned when the Sledge was not played from a Backpack" . gameTest $ \self -> do
      asEdgeOfTheEarth
      sledge <- testAssetWithDef Assets.woodenSledge (controlledBy self) self
      card <- genCard Assets.woodenSledge
      earned <- didEarnEdgeOfTheEarth KindOfAHatOnAHat
      playSledge self card
      arrive self card
      finishAction self
      attachBackpack sledge
      earned `refShouldBe` False

    -- Taking any other action in between breaks the chain.
    it "is not earned after taking a different action first" . gameTest $ \self -> do
      asEdgeOfTheEarth
      (card, sledge) <- stage self
      earned <- didEarnEdgeOfTheEarth KindOfAHatOnAHat
      playSledge self card
      arrive self card
      finishAction self
      -- A second completed action before the Sledge's ability breaks the chain.
      finishAction self
      attachBackpack sledge
      earned `refShouldBe` False

    it "is not earned attaching something that is not a Backpack" . gameTest $ \self -> do
      asEdgeOfTheEarth
      (card, sledge) <- stage self
      earned <- didEarnEdgeOfTheEarth KindOfAHatOnAHat
      playSledge self card
      arrive self card
      finishAction self
      other <- genCard Assets.smallRadio
      run $ PlaceUnderneath (toTarget sledge) [other]
      earned `refShouldBe` False

  context "Friends Forever" $ do
    let broughtSinha self =
          run
            $ HandleTargetChoice
              (toId self)
              ScenarioSource
              (CardCodeTarget $ toCardCode Assets.drMalaSinhaDaringPhysician)
        -- at, not ix: the harness campaign log starts with no partners at all, so
        -- ix would silently no-op and leave her merely Safe.
        resolute attrs =
          attrs
            & Campaign.logL
            . partnersL
            . at (toCardCode Assets.drMalaSinhaDaringPhysician)
            ?~ CampaignLogPartner 0 0 Resolute
        withResoluteSinha = do
          overTest \g -> g {gameMode = first (overAttrs resolute) (gameMode g)}
          tick

    it "is earned bringing one resolute partner to every scenario" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08648b"
      run Setup
      broughtSinha self
      withResoluteSinha
      earned <- didEarnEdgeOfTheEarth FriendsForever
      finishTheCampaign
      earned `refShouldBe` True

    it "is not earned when the partner never confronted their demons" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08648b"
      run Setup
      broughtSinha self
      earned <- didEarnEdgeOfTheEarth FriendsForever
      finishTheCampaign
      earned `refShouldBe` False

    it "is not earned when two different partners came along" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08648b"
      run Setup
      broughtSinha self
      run
        $ HandleTargetChoice
          (toId self)
          ScenarioSource
          (CardCodeTarget $ toCardCode Assets.averyClaypoolAntarcticGuide)
      withResoluteSinha
      earned <- didEarnEdgeOfTheEarth FriendsForever
      finishTheCampaign
      earned `refShouldBe` False

    -- A scenario played without them breaks "in each scenario".
    it "is not earned when a scenario was played without them" . gameTest $ \self -> do
      asEdgeOfTheEarthScenario "08648b"
      run Setup
      broughtSinha self
      overTest \g -> g {gameMode = second (overAttrs (\a -> a {Scenario.scenarioId = "08549"})) (gameMode g)}
      run Setup
      withResoluteSinha
      earned <- didEarnEdgeOfTheEarth FriendsForever
      finishTheCampaign
      earned `refShouldBe` False

  context "the chaos bag at the end of the campaign" $ do
    it "earns Hell Froze Over with no frost tokens" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth HellFrozeOver
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn The Cold Never Bothered Me Anyway with an empty bag" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      earned <- didEarnEdgeOfTheEarth TheColdNeverBotheredMeAnyway
      finishTheCampaign
      earned `refShouldBe` False

    it "earns The Cold Never Bothered Me Anyway with eight frost tokens" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      withFrostTokens 8
      earned <- didEarnEdgeOfTheEarth TheColdNeverBotheredMeAnyway
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Hell Froze Over with frost still in the bag" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      withFrostTokens 8
      earned <- didEarnEdgeOfTheEarth HellFrozeOver
      finishTheCampaign
      earned `refShouldBe` False

  context "winning the campaign" $ do
    it "earns Line in the…Snow with three active ultimatums" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship, UltimatumOfDread]
      earned <- didEarnEdgeOfTheEarth SnowLineInTheSand
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Line in the…Snow with only two" . gameTest $ \_ -> do
      asEdgeOfTheEarth
      withUltimatums [UltimatumOfFailure, UltimatumOfHardship]
      earned <- didEarnEdgeOfTheEarth SnowLineInTheSand
      finishTheCampaign
      earned `refShouldBe` False

    it "earns Antarctic Expertise on Expert" . gameTest $ \_ -> do
      asEdgeOfTheEarthWith Expert
      earned <- didEarnEdgeOfTheEarth AntarcticExpertise
      finishTheCampaign
      earned `refShouldBe` True

    it "does not earn Antarctic Expertise below Expert" . gameTest $ \_ -> do
      asEdgeOfTheEarthWith Hard
      earned <- didEarnEdgeOfTheEarth AntarcticExpertise
      finishTheCampaign
      earned `refShouldBe` False
