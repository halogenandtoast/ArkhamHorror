module Arkham.Investigator.Cards.LukeRobinsonSpec (spec) where

import TestImport.New

import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack qualified as Attack
import Arkham.Classes.HasGame (getGame)
import Arkham.Enemy.Types (Enemy)
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Investigator.Cards (lukeRobinson)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

{- | Every card-play option currently offered to any player, as card ids.

Luke injects his connecting-location plays into the /player window/ rather than through
'playCard', and the 'playCard' helper pushes 'InitiatePlayCardWithWindows' directly -- so
specs covering the window enumeration (#5422) have to inspect the pending question. Both
the engine and Luke label a card play with @targetLabel (toCardId c)@, i.e. a
'TargetLabel' over a 'CardIdTarget'.
-}
offeredCardIds :: TestAppT [CardId]
offeredCardIds = do
  questionMap <- gameQuestion <$> getGame
  pure
    [ cid
    | (_, question) <- mapToList questionMap
    , TargetLabel (CardIdTarget cid) _ <- choicesOf (stripQuestionWrappers question)
    ]
 where
  choicesOf = \case
    ChooseOne msgs -> msgs
    ChooseOneAtATime msgs -> msgs
    ChooseN _ msgs -> msgs
    PlayerWindowChooseOne msgs -> msgs
    _ -> []

{- | Luke alone at a location whose only connection holds two enemies: an event with an
enemy requirement is then playable only through his connecting-location ability.
-}
lukeAcrossFromEnemies :: Investigator -> TestAppT (Location, Location, Enemy, Enemy)
lukeAcrossFromEnemies self = do
  (location1, location2) <- testConnectedLocations id id
  self `moveTo` location1
  enemy1 <- testEnemy & prop @"fight" 0
  enemy2 <- testEnemy
  enemy1 `spawnAt` location2
  enemy2 `spawnAt` location2
  pure (location1, location2, enemy1, enemy2)

spec :: Spec
spec = describe "Luke Robinson" do
  beginsWithInPlay lukeRobinson Assets.gateBox

  it
    "You may play one event each turn as if you were at a connecting location and engaged with each enemy at that location."
    . gameTestWith lukeRobinson
    $ \self -> do
      (_, _, enemy1, _) <- lukeAcrossFromEnemies self
      etherealForm <- genCard Events.etherealForm
      etherealForm2 <- genCard Events.etherealForm
      withProp @"hand" [etherealForm, etherealForm2] self
      withProp @"resources" 4 self
      duringTurn self do
        self.playableCards `shouldMatchListM` [etherealForm, etherealForm2]
        self `playCard` etherealForm
        chooseTarget enemy1
        startSkillTest
        applyResults
        assert enemy1.exhausted
        self.playableCards `shouldReturn` []

  -- The ability refreshes at the start of each of Luke's turns; it used to stay spent for
  -- the rest of the round.
  it "refreshes each turn (#3549)" . gameTestWith lukeRobinson $ \self -> do
    (_, _, enemy1, _) <- lukeAcrossFromEnemies self
    etherealForm <- genCard Events.etherealForm
    etherealForm2 <- genCard Events.etherealForm
    withProp @"hand" [etherealForm, etherealForm2] self
    withProp @"resources" 4 self
    duringTurn self do
      self `playCard` etherealForm
      chooseTarget enemy1
      startSkillTest
      applyResults
      self.playableCards `shouldReturn` []
    duringTurn self do
      self.playableCards `shouldMatchListM` [etherealForm2]

  -- InitiatePlayCard is processed twice for a card that pays a cost (once before payment,
  -- once after); without the tracked card id Luke asked where to play it both times.
  it "only asks where to play a card once (#3396)" . gameTestWith lukeRobinson $ \self -> do
    (location1, _, enemy1, _) <- lukeAcrossFromEnemies self
    here <- testEnemy & prop @"fight" 0
    here `spawnAt` location1
    etherealForm <- genCard Events.etherealForm
    withProp @"hand" [etherealForm] self
    withProp @"resources" 4 self
    duringTurn self do
      self `playCard` etherealForm
      clickLabel "$label.playAtConnectingLocation"
      -- If Luke asked a second time this would not find an enemy to target.
      chooseTarget enemy1
      startSkillTest
      applyResults
      assert enemy1.exhausted

  context "another investigator's turn" do
    -- Luke's PlayerWindow handler used to fire for *every* addressee, injecting his own
    -- action-consuming plays into other investigators' windows.
    it "does not offer non-fast events (#5422)" . gameTestWith lukeRobinson $ \self -> do
      (location1, _, _, _) <- lukeAcrossFromEnemies self
      other <- addInvestigator Investigators.jennyBarnes
      other `moveTo` location1
      etherealForm <- genCard Events.etherealForm
      withProp @"hand" [etherealForm] self
      withProp @"resources" 4 self
      run $ BeginTurn (toId other)
      run $ PlayerWindow (toId other) [] False False
      offeredCardIds `shouldNotContainM` [etherealForm.id]

    -- ...but reaction events must still reach a connecting location -- that is the whole
    -- point of the ability in multiplayer. Dodge cancels an attack on an investigator
    -- colocated with you, and Luke is colocated with the victim only "as if". This regressed
    -- on `select (ConnectedLocation NotForMovement)` resolving through guardYourLocation
    -- (the ACTIVE investigator's location), which on Jenny's turn gave Luke the locations
    -- connected to *hers*; lukeConnectingLocations anchors it to his own.
    it "can still play reaction events for a connecting location" . gameTestWith lukeRobinson $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      other <- addInvestigator Investigators.jennyBarnes
      other `moveTo` location2
      enemy <- testEnemy & prop @"healthDamage" 1
      enemy `spawnAt` location2
      dodge <- genCard Events.dodge
      withProp @"hand" [dodge] self
      withProp @"resources" 4 self
      run $ BeginTurn (toId other)
      let attack = Attack.enemyAttack (toId enemy) enemy (toId other)
      getIsPlayable
        (toId self)
        (toSource self)
        (UnpaidCost NoAction)
        [mkWhen (Window.EnemyAttacks attack)]
        dodge
        `shouldReturn` True

  -- A granted action ("as if it were your turn") is not actually your turn: it offers the
  -- NonFast window only, so non-fast cards remain playable but "during your turn" fast
  -- cards do not (#4894).
  context "a granted (immediate) action" do
    it "offers non-fast events but not \"during your turn\" fast events" . gameTestWith lukeRobinson $ \self -> do
      (_, _, _, _) <- lukeAcrossFromEnemies self
      etherealForm <- genCard Events.etherealForm
      willToSurvive <- genCard Events.willToSurvive
      withProp @"hand" [etherealForm, willToSurvive] self
      withProp @"resources" 10 self
      run $ BeginTurn (toId self)
      run $ PlayerWindow (toId self) [] False True
      offeredCardIds `shouldContainM` [etherealForm.id]
      offeredCardIds `shouldNotContainM` [willToSurvive.id]

  context "Elder Sign" do
    it "+1" . gameTestWith lukeRobinson $ \self -> do
      self.elderSignModifier `shouldReturn` 1

    it "places 1 charge on gate box" . gameTestWith lukeRobinson $ \self -> do
      gateBox <- self `putAssetIntoPlay` Assets.gateBox
      setChaosTokens [ElderSign]
      sid <- getRandom
      run $ beginSkillTest sid self #combat 100
      startSkillTest
      applyResults
      -- 3 initial plus 1 from elder sign
      gateBox.charges `shouldReturn` 4
