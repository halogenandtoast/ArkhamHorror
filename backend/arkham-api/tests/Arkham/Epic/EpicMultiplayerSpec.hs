module Arkham.Epic.EpicMultiplayerSpec (spec) where

import Api.Arkham.Epic (applyBoundedDelta, armActAdvanceGates, bumpSharedRevision)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Epic.Types
import Arkham.Prelude
import Test.Hspec

-- A countermeasures delta with the given idempotency id and signed amount.
mkDelta :: Text -> Int -> SharedDelta
mkDelta did amount =
  SharedDelta {sharedDeltaId = did, sharedDeltaKey = Countermeasures, sharedDeltaAmount = amount}

spec :: Spec
spec = describe "Epic Multiplayer shared state" do
  -- An event of 16 investigators starting with 8 countermeasures (ceil(16/2)).
  let s0 = setSharedCounter Countermeasures 8 (emptySharedEventState 16)

  describe "applyDelta" do
    it "adds a signed amount to the counter" do
      sharedCounter Countermeasures (applyDelta (mkDelta "a" (-1)) s0) `shouldBe` 7

    it "is idempotent for a repeated delta id (double-commit / retry safe)" do
      let once = applyDelta (mkDelta "a" (-1)) s0
          twice = applyDelta (mkDelta "a" (-1)) once
      sharedCounter Countermeasures twice `shouldBe` 7

    it "applies distinct deltas independently" do
      let s = applyDelta (mkDelta "b" 2) (applyDelta (mkDelta "a" (-1)) s0)
      sharedCounter Countermeasures s `shouldBe` 9

  describe "revertDelta (cross-group undo)" do
    it "reverses one group's spend after another group also spent" do
      -- start 8; group A spends 1 (->7); group B spends 1 (->6); undo A (->7).
      -- Correct *because* additive deltas commute: undo subtracts A's amount from
      -- the current value, regardless of B's intervening spend.
      let a = mkDelta "A" (-1)
          b = mkDelta "B" (-1)
          afterBoth = applyDelta b (applyDelta a s0)
          afterUndoA = revertDelta a afterBoth
      sharedCounter Countermeasures afterBoth `shouldBe` 6
      sharedCounter Countermeasures afterUndoA `shouldBe` 7

    it "forgets the delta id so the same action can be redone" do
      let a = mkDelta "A" (-1)
          afterUndo = revertDelta a (applyDelta a s0)
          afterRedo = applyDelta a afterUndo
      sharedCounter Countermeasures afterRedo `shouldBe` 7

    it "is a no-op for a delta that was never applied" do
      sharedCounter Countermeasures (revertDelta (mkDelta "ghost" (-5)) s0) `shouldBe` 8

  describe "bounded authoritative counters" do
    it "records only effective Blob overkill so undo remains exact" do
      let key = SharedEnemyHealth (CardCode "85037")
          atTwo = setSharedCounter key 2 (emptySharedEventState 1)
          attempted = SharedDelta "overkill" key (-5)
          (after', effective) = applyBoundedDelta atTwo attempted
      sharedCounter key after' `shouldBe` 0
      sharedDeltaAmount effective `shouldBe` (-2)
      sharedCounter key (revertDelta effective after') `shouldBe` 2

    it "caps Blob healing at the global maximum" do
      let key = SharedEnemyHealth (CardCode "85037")
          atFourteen = setSharedCounter key 14 (emptySharedEventState 1)
          (after', effective) = applyBoundedDelta atFourteen (SharedDelta "heal" key 5)
      sharedCounter key after' `shouldBe` 15
      sharedDeltaAmount effective `shouldBe` 1

    it "arms the organizer gate and consumes the request atomically" do
      let state =
            setSharedCounter (AdvanceRequested 1) 1
              $ setSharedCounter (SharedActProgress 1) 4
              $ emptySharedEventState 2
          armed = armActAdvanceGates state
      sharedCounter (AwaitingOrganizer 1) armed `shouldBe` 1
      sharedCounter (AdvanceRequested 1) armed `shouldBe` 0

    it "bumps sharedVersion only for a real state change" do
      let changed = setSharedCounter Countermeasures 1 s0
      sharedVersion (bumpSharedRevision s0 changed) `shouldBe` sharedVersion s0 + 1
      bumpSharedRevision s0 s0 `shouldBe` s0

  describe "SharedKey text encoding" do
    it "round-trips every key shape" do
      let ekeys =
            [ Countermeasures
            , LeadFaction
            , SharedActProgress 3
            , GroupDoom (GroupOrdinal 2)
            , MainStreetReady (GroupOrdinal 1)
            , ReplicationPending (GroupOrdinal 3)
            ]
      for_ ekeys \k -> sharedKeyFromText (sharedKeyText k) `shouldBe` Just k

  describe "JSON round-trips (save-file stability)" do
    it "round-trips a SharedDelta" do
      let d = mkDelta "x" (-3)
      decode (encode d) `shouldBe` Just d

    it "round-trips a SharedEventState" do
      decode (encode s0) `shouldBe` Just s0
