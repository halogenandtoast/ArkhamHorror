module Arkham.Treachery.Cards.EndOfNegotiationsSpec (spec) where

import Arkham.Ability.Types (abilityIndex)
import Arkham.Act (lookupAct)
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Act)
import Arkham.Calculation (GameCalculation (Fixed))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Entities qualified as Entities
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Helpers.Placement (attachTo)
import Arkham.Helpers.SkillTest (parley)
import Arkham.Matcher
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Window (defaultWindows)
import TestImport.New

realAct :: CardDef -> TestAppT Act
realAct def = do
  card <- genCard def
  let actId' = ActId (toCardCode card)
      act' = either (error . show) id $ lookupAct actId' 1 (toCardId card)
  overTest $ entitiesL . Entities.actsL %~ insertEntity act'
  pure act'

spec :: Spec
spec = describe "End of Negotiations" $ do
  -- Real repro (game blob): act 11504 "Questioning the Gangs" in play, End of
  -- Negotiations attached to a Gang Soldier engaged with the investigator. The
  -- act's ability 1 is "[fast] Spend X clues: Parley. Discard an engaged
  -- Criminal enemy with X remaining health at your location." -- a *fast*
  -- ability carrying a bold Parley designator.
  --
  -- The EoN holder is given high health so it is NOT a legal target of the act's
  -- parley (which discards its target). Otherwise the parley would discard the
  -- holder, taking EoN out of play before the after-parley window -- in which
  -- case NOT attacking is correct, and the test would prove nothing.
  it "attacks the parleyer after the act's fast Parley ability" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 3

    eonHolder <- testEnemyWithDef Enemies.gangSoldier id & prop @"health" 10 & prop @"healthDamage" 1
    eonHolder `spawnAt` location
    eon <- genEncounterCard Cards.endOfNegotiations
    run =<< Helpers.createTreacheryAt_ (toCard eon) (attachTo (toId eonHolder))

    -- the only legal parley target: 2 remaining health < 1 + 3 clues
    victim <- testEnemyWithDef Enemies.gangSoldier id
    victim `spawnAt` location

    act <- realAct Acts.questioningTheGangsV1

    duringTurn self $ do
      [doParley] <- filter ((== 1) . abilityIndex) <$> getActionsFrom self act
      run $ UseAbility (toId self) doParley (defaultWindows $ toId self)
      -- the act's parley effect discards its chosen criminal target...
      chooseTarget victim
      -- ...and only then does the after-parley window offer EoN's forced ability.
      useForcedAbility
      -- the attached enemy attacks; resolve the damage assignment.
      applyAllDamage

    -- the parley itself resolved (guards against a vacuous pass/fail above)
    assertNone $ EnemyWithId (toId victim)
    self.damage `shouldReturn` 1

  -- Grimoire v1.0 glossary "Parley" (.claude/references/grimoire/glossary/parley.md):
  -- "Parley abilities are exclusively resolved by playing cards or activating
  -- abilities." A bare `parley` skill-test helper call is therefore NOT a Parley
  -- \*action*, opens no PerformAction window, and must NOT trigger this Forced
  -- ability. This test pins that as intended behavior, not a bug.
  it "does not attack after a bare parley skill test (not a Parley action)" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location

    gangSoldier <- testEnemyWithDef Enemies.gangSoldier id
    gangSoldier `spawnAt` location
    eon <- genEncounterCard Cards.endOfNegotiations
    run =<< Helpers.createTreacheryAt_ (toCard eon) (attachTo (toId gangSoldier))

    setChaosTokens [Zero]
    sid <- getRandom
    run $ parley sid (toId self) (TestSource mempty) (toId gangSoldier) #willpower (Fixed 1)
    startSkillTest
    click "Apply results"
    self.damage `shouldReturn` 0
