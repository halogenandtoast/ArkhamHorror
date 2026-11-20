module Arkham.Investigator.Cards.RexMurphyParallelSpec (spec) where

import Arkham.ChaosBag.Base (chaosBagChaosTokens)
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.ChaosBag (getRemainingCurseTokens)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioChaosBag))
import TestImport.New

{- | Rex prints exactly ONE [reaction] -- "when you would either place 1 of your
clues on your location, or add 2 [curse] tokens to the chaos bag: do the other
instead" -- so the engine must expose it at index 1 for BOTH directions.
@ActiveCost@ hand-rolls a stand-in label at that index when the curse pool
cannot cover an additional cost, and #6020 was that stand-in pointing at a
second index that only handled the other direction.
-}
rexReaction :: HasCallStack => TestAppT ()
rexReaction = chooseOptionMatching "use Rex's printed reaction (index 1)" \case
  AbilityLabel {ability} -> ability.index == 1 && ability.cardCode == "90078"
  _ -> False

curseInBag :: TestAppT Int
curseInBag =
  scenarioFieldMap ScenarioChaosBag (count (== CurseToken) . map chaosTokenFace . chaosBagChaosTokens)

spec :: Spec
spec = describe "Rex Murphy (Parallel)" do
  context "when 2 curse tokens would be added to the chaos bag" do
    it "places 1 of your clues on your location instead, with the bag already at 10 curse (#6020)"
      . gameTestWith Investigators.rexMurphyParallel
      $ \self -> do
        -- 10 curse in the bag is the rules maximum, which leaves the token pool
        -- empty, so Faustian Bargain's "add 2 [curse]" cost is payable ONLY
        -- through Rex's reaction. Before the fix, answering the single-option
        -- prompt fell through to the clue-direction handler, no CancelBatch was
        -- pushed, and the queued AddChaosToken hit `error "no more curse tokens"`.
        setChaosTokens (replicate 10 CurseToken)
        getRemainingCurseTokens `shouldReturn` 0
        withProp @"clues" 2 self
        location <- testLocation
        self `moveTo` location
        locationClues <- field LocationClues (toId location)

        faustianBargain <- genCard Events.faustianBargain
        self `addToHand` faustianBargain
        duringTurn self do
          self `playCard` faustianBargain
          rexReaction

        -- The conversion paid the cost: no 11th curse token exists, and one of
        -- Rex's clues moved onto his location instead.
        curseInBag `shouldReturn` 10
        getRemainingCurseTokens `shouldReturn` 0
        self.clues `shouldReturn` 1
        field LocationClues (toId location) `shouldReturn` (locationClues + 1)
        -- and the event itself resolved: 5 resources to investigators here
        self.resources `shouldReturn` 5

    it "stays optional while the token pool can pay the cost"
      . gameTestWith Investigators.rexMurphyParallel
      $ \self -> do
        -- An empty bag leaves all 10 curse in the pool, so the cost is payable
        -- the ordinary way and the reaction must be declinable.
        setChaosTokens [Zero]
        getRemainingCurseTokens `shouldReturn` 10
        withProp @"clues" 2 self
        location <- testLocation
        self `moveTo` location
        locationClues <- field LocationClues (toId location)

        faustianBargain <- genCard Events.faustianBargain
        self `addToHand` faustianBargain
        duringTurn self do
          self `playCard` faustianBargain
          skip

        -- Declining pays the printed cost: the 2 curse tokens go in and Rex
        -- keeps every clue.
        curseInBag `shouldReturn` 2
        getRemainingCurseTokens `shouldReturn` 8
        self.clues `shouldReturn` 2
        field LocationClues (toId location) `shouldReturn` locationClues
        self.resources `shouldReturn` 5

  context "when you would place 1 of your clues on your location" do
    it "adds 2 curse tokens to the chaos bag instead, keeping the clue"
      . gameTestWith Investigators.rexMurphyParallel
      $ \self -> do
        setChaosTokens [Zero]
        withProp @"clues" 2 self
        location <- testLocation
        self `moveTo` location
        locationClues <- field LocationClues (toId location)

        run $ InvestigatorPlaceCluesOnLocation self.id (TestSource mempty) 1
        rexReaction

        -- "Do the other instead": the placement is cancelled and 2 curse
        -- tokens are drawn from the pool.
        curseInBag `shouldReturn` 2
        getRemainingCurseTokens `shouldReturn` 8
        self.clues `shouldReturn` 2
        field LocationClues (toId location) `shouldReturn` locationClues
