module AH3e.Engine.Test (
  beginTest,
  toggleTestAsset,
  rollTestDice,
  chooseRerollDie,
  rerollDie,
  rerollUpTo,
  rerollOneOf,
  rerollAll,
  chooseDieToRaise,
  chooseDieToSet,
  setDieValue,
  raiseDie,
  markUsedInTest,
  finishTest,
  testPool,
  testPrompt,
) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Hooks
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids
import AH3e.Types.State
import Data.Map.Strict qualified as Map

currentTest :: GameM TestState
currentTest = fromJustNote "no test in progress" <$> use #test

beginTest :: TestState -> GameM ()
beginTest ts = do
  playing <- investigatorIsPlaying ts.investigator
  if playing
    then do
      -- a card that promised successes while the spell was being paid for has
      -- been waiting for this test to exist
      promised <- use #pendingSuccesses
      #pendingSuccesses .= 0
      let fresh =
            ts {step = DeterminePool, dice = [], chosenAssets = [], addedSuccesses = promised, usedInTest = []}
      -- A bonus that takes no hands competes with nothing, and its card states it
      -- flatly ("you get +2 strength as part of an attack action"), so it starts
      -- switched on and the prompt still lets it be switched off. Once-per-round
      -- dice ('roundBonusAssets') are a resource, so they stay opt-in.
      free <- map (\(cid, _, _) -> cid) . filter (\(_, _, hands) -> hands == 0) <$> usableTestAssets fresh
      -- a card that tests while a test is resolving interrupts it rather than
      -- replacing it, so whatever was in progress waits underneath
      use #test >>= traverse_ \outer -> #suspendedTests %= (outer :)
      #test ?= fresh {chosenAssets = free}
      testPrompt
    else resolveAfter ts 0

-- 490.2c: assets with at most two hand icons in total
usableTestAssets :: TestState -> GameM [(CardId, Int, Int)]
usableTestAssets ts = do
  i <- getInvestigator ts.investigator
  fmap catMaybes $ for i.assets \cid -> do
    b <- assetBehavior cid
    mdice <- b.testDice cid ts.investigator ts
    hands <- maybe 0 (.hands) <$> assetDef cid
    pure do
      n <- mdice
      guard (cid `notElem` i.lockedAssets)
      pure (cid, n, hands)

handsUsed :: TestState -> GameM Int
handsUsed ts = sum <$> for ts.chosenAssets \cid -> maybe 0 (.hands) <$> assetDef cid

-- 490.2d: "additional dice" effects join the pool before the roll. These are
-- once per round, so an asset already used this round is not offered again;
-- rolling marks every chosen asset used.
roundBonusAssets :: TestState -> GameM [(CardId, Int)]
roundBonusAssets ts = do
  i <- getInvestigator ts.investigator
  fmap catMaybes $ for [c | c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \c -> do
    b <- assetBehavior c
    n <- b.bonusDicePerRound c ts.investigator
    pure $ if n > 0 then Just (c, n) else Nothing

testPool :: TestState -> GameM Int
testPool ts = do
  base <- skillValue ts.investigator ts.skill
  usable <- usableTestAssets ts
  bonus <- roundBonusAssets ts
  held <- poolDeltaFor ts
  let assetDice = sum [n | (cid, n, _) <- usable, cid `elem` ts.chosenAssets]
      roundDice = sum [n | (cid, n) <- bonus, cid `elem` ts.chosenAssets]
  pure $ case ts.fixedPool of
    Just n -> max 0 (n + assetDice + roundDice + ts.bonusDice + held)
    Nothing -> max 1 (base + ts.modifier + assetDice + roundDice + ts.bonusDice + held)

-- | Dice the cards its owner holds add to or take from every pool, unasked.
poolDeltaFor :: TestState -> GameM Int
poolDeltaFor ts = do
  i <- getInvestigator ts.investigator
  sum <$> for [c | c <- i.assets, c `notElem` i.lockedAssets] \cid -> do
    b <- assetBehavior cid
    b.poolDelta cid ts.investigator ts

testPrompt :: GameM ()
testPrompt = do
  ts <- currentTest
  let iid = ts.investigator
  case ts.step of
    DeterminePool -> do
      usable <- usableTestAssets ts
      bonus <- roundBonusAssets ts
      used <- handsUsed ts
      -- a card may let its owner bring an extra hand's worth to bear
      spare <- handsAllowance ts.investigator
      pool <- testPool ts
      let toggles =
            [ Choice (CardLabel cid) [ToggleTestAsset cid]
            | (cid, _, hands) <- usable
            , cid `elem` ts.chosenAssets || used + hands <= 2 + spare
            ]
              <> [ Choice (CardLabel cid) [ToggleTestAsset cid]
                 | (cid, _) <- bonus
                 , cid `notElem` map (\(c, _, _) -> c) usable
                 ]
      chooseFor
        iid
        ("Roll " <> tshow pool <> " dice")
        (Choice (DoneLabel "Roll dice") [RollDice] : toggles)
    ManipulateDice -> do
      i <- getInvestigator iid
      let live = any (not . (.removed)) ts.dice
          focusRerolls =
            [Choice (SkillLabel s) [SpendForReroll (FocusCost s)] | live, (s, n) <- Map.toList i.focus, n > 0]
          clueRerolls = [label "Spend a clue to reroll a die" [SpendForReroll ClueCost] | live, i.clues > 0]
      freeRerolls <- fmap catMaybes $ for [c | live, c <- i.assets, c `notElem` i.lockedAssets, c `notElem` i.usedAssets] \c -> do
        b <- assetBehavior c
        pure
          $ if b.freeRerollPerRound
            then Just (Choice (CardLabel c) [MarkAssetUsed iid c, SpendForReroll (FreeReroll (SourceCard c))])
            else Nothing
      fromCards <- testOptionsFor ts
      chooseFor
        iid
        "Modify your dice"
        ( Choice (DoneLabel "Finish test") [FinishTest]
            : focusRerolls
              <> clueRerolls
              <> freeRerolls
              <> [Choice (TextLabel r.label) r.messages | r <- fromCards]
        )
    TestResolved -> pure ()

toggleTestAsset :: CardId -> GameM ()
toggleTestAsset cid = do
  ts <- currentTest
  let chosen = if cid `elem` ts.chosenAssets then filter (/= cid) ts.chosenAssets else ts.chosenAssets <> [cid]
  #test . _Just . #chosenAssets .= chosen
  testPrompt

rollTestDice :: GameM ()
rollTestDice = do
  ts <- currentTest
  pool <- testPool ts
  bonus <- dieBonusFor ts
  values <- map (+ bonus) <$> replicateM pool rollDie
  #test . _Just . #dice .= [Die v False | v <- values]
  #test . _Just . #step .= ManipulateDice
  investigatorL ts.investigator . #usedAssets %= (<> ts.chosenAssets)
  -- the threshold rides along: blessed and cursed move it, and the log is read later
  need <- successThreshold ts
  logText ("Rolled " <> tshow values <> " need " <> tshow need)
  -- a card of someone else's may answer this test (Intervene), and that is their
  -- decision, so each of them is asked before the roller carries on
  others <- filter ((/= ts.investigator) . (.id)) <$> playingInvestigators
  pushAll
    $ [CheckReactions (AnotherResolvesTest o.id ts.investigator) [] | o <- others]
    <> [ContinueTest]

chooseRerollDie :: RerollCost -> GameM ()
chooseRerollDie cost = do
  ts <- currentTest
  i <- getInvestigator ts.investigator
  raisers <-
    filterM (fmap (.raiseInsteadOfReroll) . assetBehavior) [c | c <- i.assets, c `notElem` i.usedAssets]
  let dice = [(idx, d) | (idx, d) <- zip [0 ..] ts.dice, not d.removed]
  chooseFor
    ts.investigator
    "Choose a die to reroll"
    ( [Choice (DieLabel idx d.value) [RerollDie cost idx] | (idx, d) <- dice]
        -- two dice can show the same face, so the offer names the die's position
        <> [ Choice
               (TextLabel ("Add one to die " <> tshow (idx + 1) <> " (" <> tshow d.value <> ") instead"))
               [RaiseInsteadOfReroll cost idx cid]
           | cid <- take 1 raisers
           , (idx, d) <- dice
           ]
    )

rerollDie :: RerollCost -> Int -> GameM ()
rerollDie cost idx = do
  ts <- currentTest
  payRerollCost ts.investigator cost
  v <- rollDie
  #test . _Just . #dice . ix idx . #value .= v
  case cost of
    FocusCost _ -> pushAll [CheckReactions (SpentFocusToReroll ts.investigator) [], ContinueTest]
    _ -> testPrompt

liveDice :: TestState -> [(Int, Die)]
liveDice ts = [(idx, d) | (idx, d) <- zip [0 ..] ts.dice, not d.removed]

{- | Reroll dice one at a time until they stop or run out of allowance. A card
that rerolls "any number" of dice passes the whole pool as the allowance.
-}
rerollUpTo :: Source -> Int -> GameM ()
rerollUpTo src n = do
  ts <- currentTest
  let live = liveDice ts
  if n <= 0 || null live
    then testPrompt
    else
      chooseFor ts.investigator ("Choose a die to reroll (" <> tshow n <> " left)")
        $ Choice (DoneLabel "Done rerolling") [ContinueTest]
        : [Choice (DieLabel idx d.value) [RerollOneOf src n idx] | (idx, d) <- live]

rerollOneOf :: Source -> Int -> Int -> GameM ()
rerollOneOf src n idx = do
  ts <- currentTest
  bonus <- dieBonusFor ts
  v <- rollDie
  #test . _Just . #dice . ix idx . #value .= v + bonus
  rerollUpTo src (n - 1)

-- | Reroll every die still in the pool at once, for a card that offers no choice.
rerollAll :: Source -> GameM ()
rerollAll _ = do
  ts <- currentTest
  bonus <- dieBonusFor ts
  for_ (liveDice ts) \(idx, _) -> do
    v <- rollDie
    #test . _Just . #dice . ix idx . #value .= v + bonus
  testPrompt

chooseDieToRaise :: Source -> GameM ()
chooseDieToRaise _ = do
  ts <- currentTest
  case liveDice ts of
    [] -> testPrompt
    live ->
      chooseFor ts.investigator "Choose a die to raise by one"
        $ [Choice (DieLabel idx d.value) [RaiseDie idx] | (idx, d) <- live]

{- | A card that sets a die outright (Grave Dirt's six) rather than nudging it.
Only dice still in the pool can be set, as for a raise.
-}
chooseDieToSet :: Int -> GameM ()
chooseDieToSet n = do
  ts <- currentTest
  case liveDice ts of
    [] -> testPrompt
    live ->
      chooseFor ts.investigator ("Choose a die to change to a " <> tshow n)
        $ [Choice (DieLabel idx d.value) [SetDieValue idx n] | (idx, d) <- live]

setDieValue :: Int -> Int -> GameM ()
setDieValue idx n = do
  #test . _Just . #dice . ix idx . #value .= n
  testPrompt

raiseDie :: Int -> GameM ()
raiseDie idx = do
  #test . _Just . #dice . ix idx . #value += 1
  testPrompt

markUsedInTest :: CardId -> GameM ()
markUsedInTest cid = #test . _Just . #usedInTest %= (<> [cid])

-- 490.5, Blessed/Cursed success thresholds
successThreshold :: TestState -> GameM Int
successThreshold ts = do
  blessed <- hasCondition ts.investigator "BLESSED"
  cursed <- hasCondition ts.investigator "CURSED"
  -- a card can lower the bar to four on its own (Dark Blessing), which is the
  -- blessed threshold without the blessing
  onFour <- hasAssetWith ts.investigator (.successOnFour)
  -- a sheet may say only a six counts, whatever else is in play (Rex Murphy)
  onSix <- (.successOnSix) <$> pure (investigatorBehavior ts.investigator)
  pure
    $ if onSix
      then 6
      else
        min (if onFour then 4 else 6)
          $ if
            | cursed -> 6
            | blessed -> 4
            | otherwise -> 5

finishTest :: GameM ()
finishTest = do
  ts <- currentTest
  threshold <- successThreshold ts
  extra <- extraSuccessesFor ts
  let successes =
        length [d | d <- ts.dice, not d.removed, d.value >= threshold] + ts.addedSuccesses + extra
  -- the test this one interrupted comes back before this result is resolved, so a
  -- result that boosts it lands on the right test
  waiting <- use #suspendedTests
  #test .= listToMaybe waiting
  #suspendedTests .= drop 1 waiting
  logText ("Test result: " <> tshow successes)
  spendBlessCurse ts.investigator (successes > 0)
  {- What a card left for "after resolving the test", pushed ahead of the result so
  that the result's own work, prepended next, still lands in front of it. -}
  for_ (reverse ts.riders) \(ctx, eff) -> push (ResolveEffect ctx eff)
  resolveAfter ts successes
  -- a sheet may answer a failure, behind whatever the failure itself set going
  when (successes == 0) $ pushEnd (CheckReactions (AfterFailedTest ts.investigator) [])

-- 490.5: blessed is discarded after a failed test, cursed after a passed one.
-- Pushed before 'resolveAfter' so the discard lands behind the test's own effect.
spendBlessCurse :: InvestigatorId -> Bool -> GameM ()
spendBlessCurse iid passed = do
  let (name, spent) = if passed then ("CURSED", "CURSED") else ("BLESSED", "BLESSED")
  conditionCard iid name >>= traverse_ \cid -> do
    logText (spent <> " is discarded")
    push (DiscardAsset cid)

resolveAfter :: TestState -> Int -> GameM ()
resolveAfter ts r = case ts.after of
  AfterEffect ctx onPass onFail -> push (ResolveEffect ctx {testResult = Just r} (if r > 0 then onPass else onFail))
  AfterAttack iid mid -> push (AttackDamage iid mid r)
  AfterEvade iid -> push (EvadeMonsters iid r)
  AfterResearch iid -> push (ResearchClues iid r)
  AfterWard iid sid -> push (WardRemove iid sid r)
  AfterSpell _ _ -> logText "Spell resolution not implemented"
  AfterPreventDamage -> #damagePrevented += r
  AfterExhaustMonster mid -> when (r > 0) $ push (ExhaustMonster mid)
  AfterMoveSpell iid bonus -> push (MoveStep (MoveState iid (r + bonus) 0 0 True False))
  AfterBoostTest -> do
    interrupted <- uses #test isJust
    pushAll $ AddTestSuccesses r : [ContinueTest | interrupted]
  AfterCustom src key -> case customAfterTest key of
    Just f -> f src r
    Nothing -> logText ("Missing custom test continuation: " <> key)
