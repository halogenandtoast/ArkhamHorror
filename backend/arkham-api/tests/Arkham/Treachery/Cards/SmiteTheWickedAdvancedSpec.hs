module Arkham.Treachery.Cards.SmiteTheWickedAdvancedSpec (spec) where

import Arkham.DamageEffect (nonAttack)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Smite the Wicked (Advanced)" $ do
  -- Smite the Wicked (Advanced) re-checks its host's defeat when it leaves play, because
  -- removing it drops the enemy's health by 2. When the host leaves play *because it was
  -- defeated*, that re-check used to restart the whole defeat pipeline, so every "after an
  -- enemy is defeated" trigger fired twice. Punishment counts the triggers here.
  it "does not resolve the attached enemy's defeat twice (#5242)" . gameTest $ \self -> do
    location <- testLocation
    enemyCard <- genEncounterCard Enemies.swarmOfRats
    punishmentCard <- genEncounterCard Treacheries.punishment
    run $ placedLocation location
    self `moveTo` location
    run $ SetEncounterDeck (Deck [punishmentCard, enemyCard])
    run $ drawEncounterCard self.id GameSource
    loadDeck self [Treacheries.smiteTheWickedAdvanced]
    drawCards self 1
    enemy <- selectJust AnyEnemy
    assert $ selectAny (TreacheryOnEnemy (EnemyWithId enemy))
    -- Swarm of Rats has 1 health, +2 from the attached Smite the Wicked (Advanced)
    run $ DealDamage (EnemyTarget enemy) (nonAttack (Just self.id) (TestSource mempty) 3)
    useForcedAbility
    applyAllDamage
    self.damage `shouldReturn` 1
    assertHasNoReaction
