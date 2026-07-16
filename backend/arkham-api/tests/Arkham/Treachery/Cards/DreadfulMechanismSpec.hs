module Arkham.Treachery.Cards.DreadfulMechanismSpec (spec) where

import Arkham.Ability.Types (abilityIndex)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Matcher (treacheryIs)
import Arkham.Projection
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (defaultWindows)
import TestImport.New

-- Dreadful Mechanism (70057):
--   "Forced - After you perform an action other than the [action] ability
--    below: Take 1 damage, discard an asset taking up at least 1 hand slot,
--    and discard Dreadful Mechanism."
--
-- This Forced ability keys on performing an *action*. A fast ability that
-- carries a bold action designator (Garrote Wire (2): "[fast] ... exhaust
-- Garrote Wire: Fight.") is NOT an action, so it must NOT satisfy this window.
--
-- Regression guard for commit 353b97baea: that change opened a
-- `PerformAction #after` window for *every* fast ability with a bold action
-- designator (to fix End of Negotiations vs a fast Parley). But fight/evade/
-- investigate/move already have real action windows, and forced cards keyed on
-- "perform an action" (Dreadful Mechanism, Arm Injury, Serpent's Haven, Time
-- Warp) mis-fire when a *fast* fight/evade/investigate/move opens that window.
-- Compare Arkham.Treachery.Cards.EndOfNegotiationsSpec, which pins the parley
-- direction (parley has no basic action, so fast Parley abilities SHOULD fire).
spec :: Spec
spec = describe "Dreadful Mechanism" do
  it "fires after a basic fight action (positive control)" . gameTest $ \self -> do
    updateInvestigator self $ \a -> a {investigatorCombat = 4}
    location <- testLocation
    self `moveTo` location
    enemy <- testEnemy & prop @"health" 2 & prop @"fight" 1
    enemy `spawnAt` location
    _ <- self `putTreacheryIntoPlay` Treacheries.dreadfulMechanism
    setChaosTokens [Zero]

    void $ self `fightEnemy` enemy
    startSkillTest
    click "Apply results"

    -- basic fight IS an action -> Dreadful Mechanism's Forced ability is offered
    useForcedAbility
    self.damage `shouldReturn` 1

  it "does NOT fire after a fast [fight] ability" . gameTest $ \self -> do
    updateInvestigator self $ \a -> a {investigatorCombat = 4}
    location <- testLocation
    self `moveTo` location
    -- Garrote Wire (2) targets an enemy with exactly 1 remaining health
    enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
    enemy `spawnAt` location
    garrote <- self `putAssetIntoPlay` Assets.garroteWire2
    _ <- self `putTreacheryIntoPlay` Treacheries.dreadfulMechanism
    setChaosTokens [Zero]

    [fastFight] <- filter ((== 1) . abilityIndex) <$> field AssetAbilities garrote
    run $ UseAbility (toId self) fastFight (defaultWindows $ toId self)
    chooseTarget enemy
    startSkillTest
    click "Apply results"

    -- a fast ability is not an action: Dreadful Mechanism must not fire, it
    -- stays in play, and no damage is dealt.
    self.damage `shouldReturn` 0
    selectAny (treacheryIs Treacheries.dreadfulMechanism) `shouldReturn` True
