module AH3e.Engine.Test (
  beginTest,
  toggleTestAsset,
  rollTestDice,
  rollAdditionalDice,
  rollADiePerFailure,
  removeADie,
  chooseRerollDie,
  rerollDie,
  rerollUpTo,
  rerollUpToPaying,
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
      -- likewise a rider left before this test existed (Book of Shadows)
      waiting <- fromMaybe [] <$> use #pendingRiders
      #pendingRiders .= Nothing
      -- a card may have named this test's pool outright (Anything You Can Do)
      stated <- (.fixedPoolNext) <$> getInvestigator ts.investigator
      investigatorL ts.investigator . #fixedPoolNext .= Nothing
      let fresh =
            ts
              { step = DeterminePool
              , dice = []
              , chosenAssets = []
              , addedSuccesses = promised
              , usedInTest = []
              , fixedPool = maybe stated Just ts.fixedPool
              , riders = ts.riders <> waiting
              }
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
    Just n -> max 0 (n + ts.bonusDice)
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
      fromCards <- poolOptionsFor ts
      chooseFor
        iid
        ("Roll " <> tshow pool <> " dice")
        ( Choice (DoneLabel "Roll dice") [RollDice]
            : toggles
              <> [Choice (TextLabel r.label) r.messages | r <- fromCards]
        )
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
  -- a card of somebody else's may want to match this pool later in the round
  investigatorL ts.investigator . #lastTestDice ?= pool
  -- the threshold rides along: blessed and cursed move it, and the log is read later
  need <- successThreshold ts
  logText ("Rolled " <> tshow values <> " need " <> tshow need)
  -- a card may compel a success to be rolled again; it is not offered, so the
  -- first one on the table is the one that goes back
  compelled <- hasAssetWith ts.investigator (.forcedRerollOfSuccess)
  rolled <- currentTest
  when compelled $ for_ (take 1 [idx | (idx, d) <- liveDice rolled, d.value >= need]) \idx -> do
    v <- rollDie
    #test . _Just . #dice . ix idx . #value .= v
    logText ("A success is rolled again and comes up " <> tshow v)
  -- a card of someone else's may answer this test (Intervene), and that is their
  -- decision, so each of them is asked before the roller carries on
  others <- filter ((/= ts.investigator) . (.id)) <$> playingInvestigators
  pushAll
    $ [CheckReactions (AnotherResolvesTest o.id ts.investigator) [] | o <- others]
    <> [ContinueTest]

{- | Dice joining a pool that has already been rolled, for a card that buys them
after the fact (490.2d covers the ones added before). They are rolled at once
and land beside the others, so everything that reads the pool sees them.
-}
rollAdditionalDice :: Source -> Int -> GameM ()
rollAdditionalDice _ n = do
  ts <- currentTest
  bonus <- dieBonusFor ts
  values <- map (+ bonus) <$> replicateM (max 0 n) rollDie
  #test . _Just . #dice %= (<> [Die v False | v <- values])
  need <- successThreshold ts
  logText ("Rolled " <> tshow values <> " more, need " <> tshow need)
  testPrompt

{- | "One additional die for each die that is not a success." What counts as a
success moves with blessings and cards, so the dice are counted here rather than
on the card that asks (Reckless Resolve).
-}
rollADiePerFailure :: Source -> GameM ()
rollADiePerFailure src = do
  ts <- currentTest
  need <- successThreshold ts
  rollAdditionalDice src (length [d | (_, d) <- liveDice ts, d.value < need])

{- | A die taken out of the test, which is what FATIGUED charges for a reroll.
Removing rather than discarding keeps the pool's shape, the way a spent die is
kept (490.3).
-}
removeADie :: Source -> GameM ()
removeADie _ = do
  ts <- currentTest
  let live = liveDice ts
  unless (null live)
    $ chooseFor ts.investigator "Choose a die to remove from the test"
    $ [Choice (DieLabel idx d.value) [RemoveDieAt idx] | (idx, d) <- live]

{- | What rerolling costs beyond its own price: a card may take a die out of the
test for it (FATIGUED). Charged once per reroll, however many dice it covers.
-}
rerollSurcharge :: InvestigatorId -> GameM [Message]
rerollSurcharge iid = do
  i <- getInvestigator iid
  fatigued <-
    anyM
      (\cid -> assetBehavior cid >>= \b -> b.rerollRemovesADie cid iid)
      [c | c <- i.assets, c `notElem` i.lockedAssets]
  pure [RemoveADie (SourceInvestigator iid) | fatigued]

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
  {- All of this goes out in one push: a second one would prepend in front of what
  the first left, putting ContinueTest ahead of the cards answering the reroll and
  stranding their messages past the end of the test (Chef's Knife). -}
  answered <- afterRerollFor ts.investigator idx
  surcharge <- rerollSurcharge ts.investigator
  -- queued rather than prompted, so the surcharge is settled in front of both
  spent <- case cost of
    FocusCost _ -> afterSpentFocusFor ts.investigator
    _ -> pure []
  pushAll
    $ surcharge
    <> answered
    <> case cost of
      -- the dice a card buys land behind the window, and roll the test on themselves
      FocusCost _ ->
        CheckReactions (SpentFocusToReroll ts.investigator) []
          : spent
            <> [ContinueTest | null spent]
      _ -> [ContinueTest]

liveDice :: TestState -> [(Int, Die)]
liveDice ts = [(idx, d) | (idx, d) <- zip [0 ..] ts.dice, not d.removed]

{- | Reroll dice one at a time until they stop or run out of allowance. A card
that rerolls "any number" of dice passes the whole pool as the allowance.
-}

{- | The staged rerolls, with whatever a card charges for rerolling at all paid
first. The recursion goes through 'rerollUpTo', so the surcharge is paid once.
-}
rerollUpToPaying :: Source -> Int -> GameM ()
rerollUpToPaying src n = do
  ts <- currentTest
  surcharge <- rerollSurcharge ts.investigator
  if null surcharge
    then rerollUpTo src n
    else pushAll (surcharge <> [RerollUpToNow src n])

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
  surcharge <- rerollSurcharge ts.investigator
  pushAll (surcharge <> [ContinueTest])

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
  that the result's own work, prepended next, still lands in front of it. The
  result goes in the rider's context, so a rider that only answers a failure can
  read it (Just That Good). -}
  for_ (reverse ts.riders) \(ctx, eff) ->
    push (ResolveEffect ctx {testResult = Just successes} eff)
  resolveAfter ts successes
  -- a sheet may answer a failure, behind whatever the failure itself set going
  pushEnd
    $ CheckReactions
      (if successes == 0 then AfterFailedTest ts.investigator else AfterPassedTest ts.investigator)
      []

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
  -- the result is carried past the doom it takes off, for a card that reads it
  -- rather than the doom (Scientific Method)
  AfterWard iid sid -> pushAll [WardRemove iid sid r, CheckReactions (AfterWardResult iid r) []]
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
