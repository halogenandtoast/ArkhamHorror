module Arkham.Enemy.Cards.EdgeOfTheEarth.ElderThings.GuardianElderThingSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.ElderThings qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Guardian Elder Thing" do
  {- Regression (#5798): its Forced "when this enemy is dealt damage" trigger used to eat
  the damage it fires on.

  A When window's pending effect -- here the `Damaged` that actually puts the tokens on the
  enemy -- is popped OUT of the queue by `ResolveWindowInitiations` and parked in the marker
  carried by the Forced ask, so while that ask is open the only copy lives in the question.
  `WindowAsk` queues a trailing `Do (CheckWindows ws)` behind every ask, and reaching it
  re-derives the window and REPLACES that question -- whose own pop then finds nothing,
  because the first pass already emptied the queue of it.

  In solo the seat answers the Forced ask before the re-check is ever reached, so this only
  bit in multiplayer: a second seat answering first drains the queue past it. Safeguard (2)
  is how the reported game got a second seat into the window -- as an ability, "during
  another investigator's turn" is a turn-wide condition that rides along in every window the
  turn opens (#5784). -}
  it "still deals its damage when another investigator answers the window first" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    withProp @"combat" 5 self
    -- the trigger discards the top card per damage dealt; plain test cards, so no weakness
    -- is found and the ability resolves without a prompt of its own
    cards <- testPlayerCards 3
    withProp @"deck" (Deck cards) self

    enemy <- testEnemyWithDef Enemies.guardianElderThing id
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    other `moveTo` location

    duringTurn self do
      -- after BeginTurn, so its blocking TurnBegins reaction is out of the way; from here
      -- it gives `other` a seat in every window of `self`'s turn
      other `putCardIntoPlay` Assets.safeguard2

      _ <- fightEnemy self enemy
      startSkillTest
      applyResults
      -- the When DealtDamage window parks a question on BOTH seats; `other` answers first
      skipAcrossQuestions
      useForcedAbilityAcrossQuestions

    -- 1 damage from the fight, not 0
    enemy.damage `shouldReturn` 1
