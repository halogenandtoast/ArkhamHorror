module Arkham.Enemy.EngagementSpec (spec) where

import Arkham.Attack qualified as Attack
import Arkham.Enemy.CardDefs.NightOfTheZealot.Rats qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.Exhaust (mkExhaustion)
import Arkham.Helpers.Cost (getCanAffordCost)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as AgesUnwound
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import Arkham.Phase (Phase (EnemyPhase))
import Arkham.Placement
import Arkham.Projection
import TestImport.New

-- Spawns a swarming host engaged with `self` and returns the host and its one
-- swarm card. `testEnemy` is inserted directly into the entity map, so the
-- `Swarming` spawn flow never runs and we place the swarm card by hand.
swarmingEnemyEngagedWith :: Investigator -> Location -> TestAppT (EnemyId, EnemyId)
swarmingEnemyEngagedWith self location = do
  host <- testEnemy
  host `spawnAt` location
  run $ EnemyEngageInvestigator (toId host) (toId self)
  card <- genCard Cards.swarmOfRats
  run $ PlacedSwarmCard (toId host) card
  swarm <- selectJust $ Matcher.SwarmOf (toId host)
  pure (toId host, swarm)

spec :: Spec
spec = describe "Enemy engagement" do
  for_ [(True, 1), (False, 1), (True, 3)] \(swarmFirst, swarmCount) ->
    it
      ( "splits The Myriad attacks with swarm first "
          <> show swarmFirst
          <> " and "
          <> show swarmCount
          <> " swarm cards"
      )
      . gameTest
      $ \self -> do
        other <- addInvestigator Investigators.rolandBanks
        location <- testLocation
        self `moveTo` location
        other `moveTo` location
        host <- testEnemyWithDef AgesUnwound.theMyriad id & prop @"healthDamage" 1
        run $ PlaceEnemy (toId host) (AtLocation $ toId location)
        replicateM_ swarmCount do
          card <- genCard Cards.swarmOfRats
          run $ PlacedSwarmCard (toId host) card
        swarm <- select $ Matcher.SwarmOf (toId host)
        let
          attackers = if swarmFirst then swarm <> [toId host] else toId host : swarm
          half = length attackers `div` 2
        run $ SetPhase EnemyPhase
        for_ [1 :: Int, 2] \batch -> do
          when (batch > 1) $ run $ Ready (toTarget host)
          run EnemiesAttack
          for_ (zip [0 :: Int ..] attackers) \(n, attacker) -> do
            chooseTarget attacker
            when (n == 0) useForcedAbilityAcrossQuestions
            -- Consecutive attacks on the same investigator are legal when the
            -- remaining attacks can still complete an even split.
            when (n < half) $ chooseTarget self
            applyAllDamage
          -- A new batch in the same phase must ask for the Forced again and
          -- allocate its own even split, rather than retaining the old record.
          field EnemyMeta (toId host) `shouldReturn` toJSON (False, [] :: [InvestigatorId])
          self.damage `shouldReturn` (batch * half)
          other.damage `shouldReturn` (batch * half)

  it "continues The Myriad swarm silently when an investigator is defeated during its attacks" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    withProp @"health" 2 self
    location <- testLocation
    self `moveTo` location
    other `moveTo` location
    host <- testEnemyWithDef AgesUnwound.theMyriad id & prop @"healthDamage" 1
    run $ PlaceEnemy (toId host) (AtLocation $ toId location)
    replicateM_ 4 do
      card <- genCard Cards.swarmOfRats
      run $ PlacedSwarmCard (toId host) card
    swarm <- select $ Matcher.SwarmOf (toId host)
    run $ SetPhase EnemyPhase
    run EnemiesAttack
    for_ (zip [0 :: Int ..] (toId host : swarm)) \(n, attacker) -> do
      chooseTarget attacker
      when (n == 0) useForcedAbilityAcrossQuestions
      case n of
        0 -> chooseTarget self
        1 -> chooseTarget other
        2 -> chooseTarget self
        _ -> pure () -- Only the survivor remains: no Forced or target click.
      applyAllDamage
    self.defeated `shouldReturn` True
    other.damage `shouldReturn` 3
    assertNone $ Matcher.enemyIs AgesUnwound.theMyriad <> Matcher.ReadyEnemy
    field EnemyMeta (toId host) `shouldReturn` toJSON (False, [] :: [InvestigatorId])

  it "finishes The Myriad attack batch when a pending swarm card is exhausted" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    location <- testLocation
    self `moveTo` location
    other `moveTo` location
    host <- testEnemyWithDef AgesUnwound.theMyriad id & prop @"healthDamage" 1
    run $ PlaceEnemy (toId host) (AtLocation $ toId location)
    card <- genCard Cards.swarmOfRats
    run $ PlacedSwarmCard (toId host) card
    swarm <- selectJust $ Matcher.SwarmOf (toId host)
    run $ SetPhase EnemyPhase
    run
      $ EnemyAttacks
        [ EnemyAttack
            $ (Attack.enemyAttack host host self)
              { Attack.attackAfter = [Exhaust $ mkExhaustion GameSource $ toTarget swarm]
              }
        , EnemyAttack $ Attack.enemyAttack swarm host self
        ]
    chooseTarget host
    useForcedAbilityAcrossQuestions
    chooseTarget self
    applyAllDamage
    -- The queued swarm attack is skipped because it is now exhausted, but
    -- the batch's completion message must still reach the host.
    assertAny $ Matcher.EnemyWithId swarm <> Matcher.ExhaustedEnemy
    chooseTarget swarm
    field EnemyMeta (toId host) `shouldReturn` toJSON (False, [] :: [InvestigatorId])
    self.damage `shouldReturn` 1
    other.damage `shouldReturn` 0

  it "keeps Blessed of Aforgomon beside the agenda and allows fighting and evading it" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    enemy <- testEnemyWithDef AgesUnwound.blessedOfAforgomon id
    enemy `spawnAt` location
    useForcedAbility
    field EnemyPlacement (toId enemy) `shouldReturn` NextToAgenda
    duringTurn self do
      assertAny $ Matcher.enemyIs AgesUnwound.blessedOfAforgomon <> Matcher.CanFightEnemy (toSource self)
      assertAny $ Matcher.enemyIs AgesUnwound.blessedOfAforgomon <> Matcher.CanEvadeEnemy (toSource self)
    withProp @"remainingActions" 2 self
    getCanAffordCost self.id (TestSource mempty) [] [] (ActionCost 1) `shouldReturn` True
    getCanAffordCost self.id (TestSource mempty) [] [] (ActionCost 2) `shouldReturn` False
    withProp @"remainingActions" 1 self
    getCanAffordCost self.id (TestSource mempty) [] [] (ActionCost 1) `shouldReturn` False
    run $ Exhaust (mkExhaustion GameSource $ toTarget enemy)
    getCanAffordCost self.id (TestSource mempty) [] [] (ActionCost 1) `shouldReturn` True
    self.remainingActions `shouldReturn` 1
    run $ Ready (toTarget enemy)
    assertAny $ Matcher.EnemyWithId (toId enemy) <> Matcher.ReadyEnemy
    getCanAffordCost self.id (TestSource mempty) [] [] (ActionCost 1) `shouldReturn` False
    getCanAffordCost self.id (TestSource mempty) [] [] (ActionCost 0) `shouldReturn` True
    self.remainingActions `shouldReturn` 1

  -- Regression for issue #5292 (Redeem a Former Colleague moving an engaged
  -- Edwin Bennet). `EnemyEntered` used to preserve an `InThreatArea` placement
  -- and let `Do (EnemyMove)` rewrite it, but that write runs *after* the
  -- after-enters windows — so `EnemyCheckEngagement` saw a still-engaged enemy,
  -- bailed, and the enemy landed at its destination engaged with nobody.
  it "engages an investigator at the destination when a card effect moves it out of a threat area"
    . gameTest
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      location1 <- testLocation
      location2 <- testLocation

      self `moveTo` location1
      other `moveTo` location2

      enemy <- testEnemy
      enemy `spawnAt` location1
      run $ EnemyEngageInvestigator (toId enemy) (toId self)
      self.engagedEnemies `shouldReturn` [toId enemy]

      run $ EnemyMove (toId enemy) (toId location2)

      enemy.location `shouldReturn` Just (toId location2)
      self.engagedEnemies `shouldReturn` []
      other.engagedEnemies `shouldReturn` [toId enemy]

  -- Regression for issue #5332 (The Miskatonic Museum act 2b placing the ready
  -- Hunting Horror in the Restricted Hall). Spawning engages through the
  -- `EnemySpawn` flow and moving engages through `After (EnemyEntered)`, but a
  -- bare `PlaceEnemy` of an *already in play* enemy checked nothing — so the
  -- enemy sat ready and unengaged in the investigator's location, which also
  -- swallowed Eon Chart's second basic action (evade was not performable).
  it "engages an investigator when a card effect places an in-play enemy at their location"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2
      self.engagedEnemies `shouldReturn` []

      run $ PlaceEnemy (toId enemy) (AtLocation (toId location1))

      enemy.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` [toId enemy]

  -- Regression for issue #5365 (Shades of Suffering act 1, "The Lady with the
  -- Red Parasol"). `Do (EngageEnemy)` queues the `#when EnemyEngaged` window and
  -- the threat-area placement together, so everything that window resolves
  -- happens *before* the placement — there, the act advance that returns Tzu San
  -- Niang to the shadows and redistributes her concealed mini-cards. The stale
  -- placement then dragged her back out of the shadows into the investigator's
  -- threat area, leaving her on the board twice (mini-card *and* enemy).
  it "cancels a queued engagement when the enemy is returned to the shadows"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2

      -- the shape `Do (EngageEnemy)` leaves behind once its `#when EnemyEngaged`
      -- window returns the enemy to the shadows
      pushAndRunAll
        [PlaceEnemy (toId enemy) InTheShadows, PlaceEnemy (toId enemy) (InThreatArea (toId self))]

      field EnemyPlacement (toId enemy) `shouldReturn` InTheShadows
      self.engagedEnemies `shouldReturn` []

  -- The other half of #5365. Exposing a concealed enemy moves it out of the
  -- shadows, but `Do (EnemyMove)` writes the placement *after* the enter-windows
  -- — so while act 1 advances inside those windows the enemy is still
  -- `InTheShadows`, its "return her to the shadows" is a no-op, and the move
  -- then landed her at the mini-card's location regardless.
  it "cancels an in-flight move when the enemy is returned to the shadows"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2
      run $ PlaceEnemy (toId enemy) InTheShadows

      -- the exposure's deferred placement write, with the act's return-to-the-
      -- shadows resolving ahead of it
      pushAndRunAll
        [PlaceEnemy (toId enemy) InTheShadows, Do (EnemyMove (toId enemy) (toId location1))]

      field EnemyPlacement (toId enemy) `shouldReturn` InTheShadows

  -- ...but an exposure that is *not* cancelled still commits its placement.
  it "still commits an exposure move that was not cancelled"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2
      run $ PlaceEnemy (toId enemy) InTheShadows

      run $ EnemyMove (toId enemy) (toId location1)

      enemy.location `shouldReturn` Just (toId location1)

  -- The cancel above must stay scoped to a *pending* engagement: Ghost Light 2b
  -- flips Tzu San Niang and has her engage the lead investigator straight out of
  -- the shadows, so an engagement that starts after the enemy is already there
  -- still resolves.
  it "still engages an investigator with an enemy that starts in the shadows"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2
      run $ PlaceEnemy (toId enemy) InTheShadows

      run $ EnemyEngageInvestigator (toId enemy) (toId self)

      field EnemyPlacement (toId enemy) `shouldReturn` InThreatArea (toId self)
      self.engagedEnemies `shouldReturn` [toId enemy]

  -- Regression for issue #5432 ("Get over here!" on a Disciple of the Devourer
  -- wearing Mask of Umôrdhoth). A card effect that engages an enemy standing at
  -- another location queues [PlaceEnemy (InThreatArea iid), EnemyEntered], and
  -- `EnemyEntered` rewrote that placement straight back to `AtLocation`.
  -- `EnemyCheckEngagement` in the after-enters window re-engaged ordinary enemies
  -- and hid the damage, but it bails on aloof enemies — so the enemy was pulled
  -- in and fought while engaged with nobody.
  it "engages an aloof enemy that a card effect pulls in from another location"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2
      run =<< gameModifier (TestSource mempty) (toTarget enemy) #aloof

      run $ EnemyEngageInvestigator (toId enemy) (toId self)

      field EnemyPlacement (toId enemy) `shouldReturn` InThreatArea (toId self)
      self.engagedEnemies `shouldReturn` [toId enemy]

  -- The engagement check has to stay guarded: an exhausted enemy does not engage
  -- (RR "Enemy Cards" — only a *ready*, unengaged enemy engages).
  it "leaves an exhausted enemy unengaged when it is placed at an investigator's location"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      enemy <- testEnemy
      enemy `spawnAt` location2
      exhaust enemy

      run $ PlaceEnemy (toId enemy) (AtLocation (toId location1))

      enemy.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` []

  -- Regression for issue #5500 (Luke Robinson's Gate Box). The ability disengages
  -- every enemy engaged with him and then moves him to the Dream-Gate, leaving the
  -- enemy ready and unengaged at a location that still held another investigator.
  -- Nothing rechecked engagement until `After (EndTurn _)`.
  it "engages an investigator left behind when the mover disengages and leaves"
    . gameTest
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      location1 <- testLocation
      location2 <- testLocation

      self `moveTo` location1
      other `moveTo` location2

      enemy <- testEnemy
      enemy `spawnAt` location1
      run $ EnemyEngageInvestigator (toId enemy) (toId self)
      other `moveTo` location1

      run $ DisengageEnemy (toId self) (toId enemy)
      self `moveTo` location2

      enemy.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` []
      other.engagedEnemies `shouldReturn` [toId enemy]

  -- The same guard as the placement case: an evaded (exhausted) enemy left behind
  -- stays unengaged.
  it "leaves an exhausted enemy unengaged when the mover disengages and leaves"
    . gameTest
    $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      location1 <- testLocation
      location2 <- testLocation

      self `moveTo` location1
      other `moveTo` location2

      enemy <- testEnemy
      enemy `spawnAt` location1
      run $ EnemyEngageInvestigator (toId enemy) (toId self)
      other `moveTo` location1

      exhaust enemy
      run $ DisengageEnemy (toId self) (toId enemy)
      self `moveTo` location2

      enemy.location `shouldReturn` Just (toId location1)
      other.engagedEnemies `shouldReturn` []

  -- Regression for issue #5313 (Virescent Rot on a Nightriders host). Engaged
  -- swarm cards are reported alongside their host, and their
  -- `EnemyEnteredFollowing` redirects to the host — so a host that had just
  -- been left behind by `DisengageEnemy` got dragged to the destination and
  -- immediately re-engaged.
  it "leaves a swarming host and its swarm behind when the host cannot move"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      (host, swarm) <- swarmingEnemyEngagedWith self location1
      run =<< gameModifier (TestSource mempty) (toTarget host) CannotMove

      self `moveTo` location2

      host.location `shouldReturn` Just (toId location1)
      swarm.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` []

  -- Swarming X: "The host enemy and all of its swarm cards move, engage, and
  -- exhaust as a single entity", so a swarm card that cannot move holds the
  -- whole group back.
  it "leaves a swarming host behind when one of its swarm cards cannot move"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      (host, swarm) <- swarmingEnemyEngagedWith self location1
      run =<< gameModifier (TestSource mempty) (EnemyTarget swarm) CannotMove

      self `moveTo` location2

      host.location `shouldReturn` Just (toId location1)
      swarm.location `shouldReturn` Just (toId location1)
      self.engagedEnemies `shouldReturn` []

  it "moves a swarming host and its swarm along with the investigator"
    . gameTest
    $ \self -> do
      location1 <- testLocation
      location2 <- testLocation
      self `moveTo` location1

      (host, swarm) <- swarmingEnemyEngagedWith self location1

      self `moveTo` location2

      host.location `shouldReturn` Just (toId location2)
      swarm.location `shouldReturn` Just (toId location2)
      engaged <- self.engagedEnemies
      sort engaged `shouldBe` sort [host, swarm]
