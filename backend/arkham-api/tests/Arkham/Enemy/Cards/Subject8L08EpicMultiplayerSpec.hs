module Arkham.Enemy.Cards.Subject8L08EpicMultiplayerSpec (spec) where

import Arkham.DamageEffect (nonAttack)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage, EnemyHealth))
import Arkham.Epic.Types (SharedKey (..))
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Projection (field)
import Arkham.Scenario.Types (Field (ScenarioTokens))
import Arkham.ScenarioLogKey (ScenarioCountKey (EpicShared))
import Arkham.Token
import TestImport.New

{- | Epic Multiplayer Milestone 2 for The Blob That Ate Everything.

Subject 8L-08's health and the scenario's countermeasures are event-wide SHARED
pools. The authoritative copy lives on the locked epic-event row; at the start of
every action @updateGame@ mirrors the current values into this group's scenario
state as @EpicShared@ counts (here we set them directly). Group engines are
write-only toward the pools: they emit invertible @SpendShared@ / @RaiseShared@
deltas (captured by the run loop only when an 'EpicEnv' is present, so they are
inert no-ops in this harness — but the messages are still emitted and observable
via a message checker).
-}
spec :: Spec
spec = describe "Subject 8L-08 (Epic Multiplayer)" do
  it "shows the fixed max health and surfaces the shared pool as damage tokens"
    . scenarioTest "85001"
    $ \_ -> do
      subject <- testEnemyWithDef Enemies.subject8L08EpicMultiplayer id
      -- The frozen total is mirrored first; health is the fixed max = 15 * total.
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      field EnemyHealth (toId subject) `shouldReturn` Just 30
      -- Remaining is mirrored next; damage tokens = accumulated = max - remaining.
      run $ ScenarioCountSet (EpicShared "enemy-health:85037") 25
      field EnemyDamage (toId subject) `shouldReturn` 5

  it "drains the global health pool by the damage dealt rather than dying locally"
    . scenarioTest "85001"
    $ \_ -> do
      subject <- testEnemyWithDef Enemies.subject8L08EpicMultiplayer id
      run $ ScenarioCountSet (EpicShared "total-investigators") 2
      run $ ScenarioCountSet (EpicShared "enemy-health:85037") 30
      drained <- createMessageChecker \case
        SpendShared (SharedEnemyHealth cc) 5 -> cc == "85037"
        _ -> False
      run $ DealDamage (toTarget subject) (nonAttack Nothing (TestSource mempty) 5)
      drained `refShouldBe` True
      -- 5 < 30 remaining: this group's copy is NOT defeated by its own local
      -- damage (accumulated 0 + 5 < max 30); the global pool decides defeat.
      field EnemyHealth (toId subject) `shouldReturn` Just 30

  it "reconciles shared countermeasures and emits deltas on gain and spend"
    . scenarioTest "85001"
    $ \_ -> do
      overTest $ setInitialScenarioMeta "epicMultiplayer" True
      -- Start of action: the shared value is mirrored in; local Resource tokens
      -- reconcile to it so the existing token-cost UI shows the global count.
      run $ ScenarioCountSet (EpicShared "countermeasures") 3
      scenarioFieldMap ScenarioTokens (countTokens Resource) `shouldReturn` 3

      raised <- createMessageChecker \case
        RaiseShared Countermeasures 1 -> True
        _ -> False
      run $ PlaceTokens (TestSource mempty) ScenarioTarget Resource 1
      raised `refShouldBe` True
      scenarioFieldMap ScenarioTokens (countTokens Resource) `shouldReturn` 4

      spent <- createMessageChecker \case
        SpendShared Countermeasures 1 -> True
        _ -> False
      run $ RemoveTokens (TestSource mempty) ScenarioTarget Resource 1
      spent `refShouldBe` True
      scenarioFieldMap ScenarioTokens (countTokens Resource) `shouldReturn` 3
