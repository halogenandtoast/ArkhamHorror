module Arkham.Game.ChooseDecksSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Decklist.Type qualified as Decklist
import Arkham.Game.State
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import Arkham.Projection (field)
import Arkham.Question
import Arkham.SimultaneousAsk
import Data.Aeson (Result (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Entity.Answer
import TestImport.New

-- | Regression for #5151. In the Thick of It parks deck initialization on a
-- ChooseAmounts to distribute 2 trauma. Resolving it drains the queue without
-- reaching another Ask, and gameQuestion is only ever written by
-- toExternalGame (from the Ask/AskMap branches of runMessages), so the answered
-- question used to survive on the game. The client re-posed it, and since
-- gameScenarioSteps had not advanced either, the questionVersion staleness
-- guard in Entity.Answer accepted the resubmit and suffered the trauma again.
-- The reporter's game stacked 11 resubmissions and drove Dexter Drake insane.
-- Fixed by having ClearUI (pushed once per accepted answer) consume the parked
-- question; the answer branches re-ask any seats still owed one via AskMap.
spec :: Spec
spec = describe "deck selection" do
  it "clears the trauma question once the answer resolves" . gameTest $ \self -> do
    initDeckWith self
    answerTrauma self 0 2

    field InvestigatorMentalTrauma (toId self) `shouldReturn` 2
    (gameQuestion <$> getGame) `shouldReturn` mempty

  it "does not suffer the trauma twice when the same answer is replayed" . gameTest $ \self -> do
    initDeckWith self
    g <- getGame
    answerTrauma self 0 2
    -- Replay the answer built against the pre-answer game, exactly as a client
    -- re-posing the stale question would. It must no longer resolve.
    replayTraumaAnswer g self 0 2

    field InvestigatorMentalTrauma (toId self) `shouldReturn` 2

  -- DoneChoosingDecks (which flips IsChooseDecks -> IsActive) lives only in the
  -- persisted step queue, parked behind the ChooseDeck ask. If that queue is
  -- lost, the game used to stay in IsChooseDecks forever once the last deck
  -- question resolved. runMessages now self-heals: draining while choosing
  -- decks with no deck question parked re-pushes DoneChoosingDecks.
  it "self-heals when the deck-selection continuation is lost" . gameTest $ \self -> do
    pid <- getPlayer (toId self)
    run $ SetGameState (IsChooseDecks [pid])
    (gameGameState <$> getGame) `shouldReturn` IsActive

  it "does not self-heal while a deck question is still parked" . gameTest $ \self -> do
    pid <- getPlayer (toId self)
    run $ Run [SetGameState (IsChooseDecks [pid]), AskMap (singletonMap pid ChooseDeck)]
    (gameGameState <$> getGame) `shouldReturn` IsChooseDecks [pid]

  -- Regression for #5173. Two-player Dunwich start where one deck holds In the
  -- Thick of It (PurchaseAnyTrauma 2). That trauma split is an *interactive*
  -- deck-init choice, and every InitDeck resolves while decks are still being
  -- chosen, so the Ask gets folded into the shared deck-selection AskMap instead
  -- of parking on its own. Folding it releases the rest of the deck-init tail
  -- immediately and lets DoneChoosingDecks -- queued behind that AskMap along
  -- with the campaign prologue step -- run even though the second seat never
  -- chose a deck. The reporter's game then soft-locked on a ContinueCampaign
  -- question during round 1 of The House Always Wins, with no action menu.
  --
  -- These specs deliberately do NOT assert where the choice is asked: the fix is
  -- free to defer it (as Boon of the Morrígan already does). They only assert the
  -- barrier's observable contract -- deck selection cannot finish while a seat
  -- still owes a deck, and no seat may be dropped on the way.
  context "with two players" do
    it "does not finish deck selection while another seat still owes a deck" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      selfPid <- getPlayer (toId self)
      otherPid <- getPlayer (toId other)
      beginTwoPlayerDeckSelection self [selfPid, otherPid]

      answerTraumaIfAsked self 1 1

      -- `other` has still not chosen a deck, so the barrier must hold.
      (gameGameState <$> getGame) `shouldSatisfyM` isChooseDecks

    it "keeps the other seat's deck prompt while a seat resolves a deck-init choice" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      selfPid <- getPlayer (toId self)
      otherPid <- getPlayer (toId other)
      beginTwoPlayerDeckSelection self [selfPid, otherPid]

      answerTraumaIfAsked self 1 1

      -- The other seat never chose a deck, so its prompt must survive intact
      -- rather than being clobbered by the first seat's deck-init choice.
      otherQuestion <- lookup otherPid . gameQuestion <$> getGame
      (stripQuestionWrappers <$> otherQuestion) `shouldBe` Just ChooseDeck

    -- The deck-init tail (SufferTrauma + GainXP/SpendXP) must land on the seat
    -- that bought the card and nowhere else, and neither seat may be dropped.
    -- Driven outside the barrier so it stays true however #5173 is fixed.
    it "applies the trauma and spends the granted xp on that seat only" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks

      assertRunsMessage (SpendXP (toId self) 3) do
        initDeckWith self
        answerTrauma self 1 1

      field InvestigatorPhysicalTrauma (toId self) `shouldReturn` 1
      field InvestigatorMentalTrauma (toId self) `shouldReturn` 1
      field InvestigatorPhysicalTrauma (toId other) `shouldReturn` 0
      field InvestigatorMentalTrauma (toId other) `shouldReturn` 0

      investigators <- select Matcher.Anyone
      investigators `shouldSatisfy` \iis -> toId self `elem` iis && toId other `elem` iis

  -- The specs above drive the game with a raw AskMap, which never populates
  -- gameSimultaneousAsks -- so DeferPastSimultaneousAsk takes its no-barrier
  -- fallback and they only ever exercise the pre-barrier path. These open a real
  -- barrier (BeginSimultaneousAsk), which is what makes `barrierSeat` find a seat
  -- and the deferral actually happen. See "Arkham.SimultaneousAsk".
  context "the deck-selection barrier" do
    -- The headline invariant: JoinAll releases exactly when saPending empties, so
    -- release is a pure function of state -- independent of answer order, of
    -- interleaving, and of where the queue happened to drain to.
    it "holds until every seat has resolved, in either order, and releases exactly once" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      selfPid <- getPlayer (toId self)
      otherPid <- getPlayer (toId other)
      bid <- getRandom
      openBarrier bid [selfPid, otherPid] [releasedSentinel self]

      pendingSeats bid `shouldReturn` setFromList [selfPid, otherPid]

      withEach [(selfPid, otherPid), (otherPid, selfPid)] \(firstSeat, secondSeat) -> do
        run $ SeatResolved bid firstSeat
        pendingSeats bid `shouldReturn` setFromList [secondSeat]
        (gameGameState <$> getGame) `shouldSatisfyM` isChooseDecks
        self.resources `shouldReturn` 0

        run $ SeatResolved bid secondSeat
        pendingSeats bid `shouldReturn` mempty
        (gameGameState <$> getGame) `shouldReturn` IsActive
        -- 1, not 2: the continuation fired on the join, not once per seat.
        self.resources `shouldReturn` 1

    -- A seat resolving twice must not double-fire the continuation (the client can
    -- resubmit; SeatResolved on a released barrier is a no-op).
    it "ignores a seat that resolves twice" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      selfPid <- getPlayer (toId self)
      otherPid <- getPlayer (toId other)
      bid <- getRandom
      openBarrier bid [selfPid, otherPid] [releasedSentinel self]

      run $ SeatResolved bid selfPid
      run $ SeatResolved bid selfPid
      -- still waiting on `other`, and the repeat released nothing
      pendingSeats bid `shouldReturn` setFromList [otherPid]
      (gameGameState <$> getGame) `shouldSatisfyM` isChooseDecks
      self.resources `shouldReturn` 0

      run $ SeatResolved bid otherPid
      run $ SeatResolved bid otherPid
      self.resources `shouldReturn` 1

    -- #5173 proper. The regression that matters: a seat's deck-init tail (and the
    -- campaign behind it) must be unreachable while any seat is still pending.
    it "cannot drain a seat's deck-init tail while another seat is still pending" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      selfPid <- getPlayer (toId self)
      otherPid <- getPlayer (toId other)
      bid <- getRandom
      openBarrier bid [selfPid, otherPid] [releasedSentinel self]

      initDeckWith self

      -- In the Thick of It's trauma split is interactive, so it must be deferred
      -- out of the barrier window rather than folded into the shared deck prompt.
      (gameQuestion <$> getGame) `shouldSatisfyM` (not . any isTraumaQuestion)
      traumaOf self `shouldReturn` (0, 0)

      -- The other seat answering must not drain this seat's deferred tail.
      run $ SeatResolved bid otherPid
      traumaOf self `shouldReturn` (0, 0)
      (gameGameState <$> getGame) `shouldSatisfyM` isChooseDecks
      self.resources `shouldReturn` 0

      -- This seat resolves, so the join releases its deferred setup -- which now
      -- parks on its own trauma choice. The campaign still must not move on with
      -- that setup outstanding.
      run $ SeatResolved bid selfPid
      (gameGameState <$> getGame) `shouldSatisfyM` isChooseDecks
      self.resources `shouldReturn` 0

      -- Only once the deferred setup finishes does the continuation run.
      assertRunsMessage (SpendXP (toId self) 3) $ answerTrauma self 1 1
      traumaOf self `shouldReturn` (1, 1)
      (gameGameState <$> getGame) `shouldReturn` IsActive
      self.resources `shouldReturn` 1

    it "asks for purchased trauma through the real deck-load flow" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      bid <- getRandom
      openBarrier bid [pid] []

      run $ Run [LoadDecklist pid inTheThickOfItDecklist, SeatResolved bid pid]

      game <- getGame
      let investigator = fromJustNote "deck load creates the investigator" $ find ((== pid) . attr investigatorPlayerId) (toList $ gameInvestigators game)
      (stripQuestionWrappers <$> lookup pid (gameQuestion game)) `shouldSatisfy` \case
        Just ChooseAmounts {} -> True
        _ -> False
      traumaOf investigator `shouldReturn` (0, 0)

      answerTrauma investigator 1 1
      traumaOf investigator `shouldReturn` (1, 1)
      (gameGameState <$> getGame) `shouldReturn` IsActive

    it "resolves purchased trauma for every loaded deck" . gameTest $ \self -> do
      other <- addInvestigator Investigators.rolandBanks
      selfPid <- getPlayer (toId self)
      otherPid <- getPlayer (toId other)
      bid <- getRandom
      openBarrier bid [selfPid, otherPid] []

      run $ Run [LoadDecklist selfPid inTheThickOfItDecklist, SeatResolved bid selfPid]
      run $ Run [LoadDecklist otherPid daisyInTheThickOfItDecklist, SeatResolved bid otherPid]

      game <- getGame
      let investigatorFor pid = fromJustNote "deck load creates the investigator" $ find ((== pid) . attr investigatorPlayerId) (toList $ gameInvestigators game)
          self' = investigatorFor selfPid
          other' = investigatorFor otherPid
      (stripQuestionWrappers <$> lookup selfPid (gameQuestion game)) `shouldSatisfy` \case
        Just ChooseAmounts {} -> True
        _ -> False

      answerTrauma self' 1 1
      gameAfterFirst <- getGame
      (stripQuestionWrappers <$> lookup otherPid (gameQuestion gameAfterFirst)) `shouldSatisfy` \case
        Just ChooseAmounts {} -> True
        _ -> False

      answerTrauma other' 2 0
      traumaOf self' `shouldReturn` (1, 1)
      traumaOf other' `shouldReturn` (2, 0)
      (gameGameState <$> getGame) `shouldReturn` IsActive

    -- chooseDecksWithAi is the real entry point. An AI seat is loaded in place and
    -- never prompted, so it must never become a slot -- a barrier waiting on a seat
    -- that is never asked would hang the table forever.
    it "never puts an ai seat in the barrier" $ do
      bid <- getRandom :: IO BatchId
      humanPid <- getRandom
      aiPid <- getRandom
      barrierSlotsOf (chooseDecksWithAi bid [humanPid, aiPid] [(aiPid, aiDecklist)] [])
        `shouldBe` Just (singletonMap humanPid ChooseDeck)

    -- With no human seats the barrier is joined on creation, so it never lands in
    -- state and the continuation runs immediately.
    it "opens no seats at all when every seat is ai" $ do
      bid <- getRandom :: IO BatchId
      aiPid <- getRandom
      barrierSlotsOf (chooseDecksWithAi bid [aiPid] [(aiPid, aiDecklist)] []) `shouldBe` Just mempty

    -- The continuation is durable state rather than a queued message precisely so
    -- it survives a reload mid-deck-selection.
    it "keeps an open barrier across a save/load" . gameTest $ \self -> do
      selfPid <- getPlayer (toId self)
      bid <- getRandom
      openBarrier bid [selfPid] []

      (roundTrip <$> getGame) >>= \case
        Error e -> expectationFailure e
        Success g -> map saPending (toList (gameSimultaneousAsks g)) `shouldBe` [setFromList [selfPid]]

    -- gameSimultaneousAsks parses with `.:? .!= mempty`, so games persisted before
    -- the barrier existed (no such key) must still load.
    it "loads a game persisted before the barrier existed" . gameTest $ \self -> do
      selfPid <- getPlayer (toId self)
      bid <- getRandom
      openBarrier bid [selfPid] []

      (roundTripWithout "gameSimultaneousAsks" <$> getGame) >>= \case
        Error e -> expectationFailure e
        Success g -> gameSimultaneousAsks g `shouldBe` mempty
 where
  -- Opens a REAL barrier. We drive BeginSimultaneousAsk directly rather than
  -- `chooseDecks`, which also pushes ChoosingDecks -- that wipes every
  -- investigator and re-derives the seat list from gamePlayers, neither of which a
  -- test game survives (addInvestigator does not touch gamePlayers). Everything
  -- under test here -- slots, deferral, join, continuation -- is identical; only
  -- the investigator wipe is skipped. DoneChoosingDecks heads the continuation
  -- exactly as `chooseDecks` builds it.
  openBarrier :: BatchId -> [PlayerId] -> [Message] -> TestAppT ()
  openBarrier bid pids continuation =
    run
      $ Run
        [ SetGameState (IsChooseDecks pids)
        , BeginSimultaneousAsk bid JoinAll (mapFromList [(p, ChooseDeck) | p <- pids])
            $ DoneChoosingDecks
            : continuation
        ]

  -- A countable, observable stand-in for the campaign tail that follows
  -- DoneChoosingDecks. Resources make "ran exactly once" visible as 1 vs 2.
  releasedSentinel :: Investigator -> Message
  releasedSentinel i = TakeResources (toId i) 1 (TestSource mempty) False

  pendingSeats :: BatchId -> TestAppT (Set PlayerId)
  pendingSeats bid = maybe mempty saPending . lookup bid . gameSimultaneousAsks <$> getGame

  traumaOf :: Investigator -> TestAppT (Int, Int)
  traumaOf i =
    (,) <$> field InvestigatorPhysicalTrauma (toId i) <*> field InvestigatorMentalTrauma (toId i)

  isTraumaQuestion :: Question Message -> Bool
  isTraumaQuestion q = case stripQuestionWrappers q of
    ChooseAmounts {} -> True
    _ -> False

  barrierSlotsOf :: Message -> Maybe (Map PlayerId (Question Message))
  barrierSlotsOf = \case
    Run msgs -> listToMaybe [slots | BeginSimultaneousAsk _ _ slots _ <- msgs]
    _ -> Nothing

  inTheThickOfItDecklist :: Decklist.ArkhamDBDecklist
  inTheThickOfItDecklist =
    aiDecklist
      { Decklist.slots = singletonMap "08125" 1
      }

  daisyInTheThickOfItDecklist :: Decklist.ArkhamDBDecklist
  daisyInTheThickOfItDecklist =
    inTheThickOfItDecklist
      { Decklist.investigator_code = "01002"
      , Decklist.investigator_name = "Daisy Walker"
      }

  aiDecklist :: Decklist.ArkhamDBDecklist
  aiDecklist =
    Decklist.ArkhamDBDecklist
      { Decklist.slots = mempty
      , Decklist.sideSlots = mempty
      , Decklist.investigator_code = "01001"
      , Decklist.investigator_name = "Roland Banks"
      , Decklist.meta = Nothing
      , Decklist.taboo_id = Nothing
      , Decklist.url = Nothing
      , Decklist.decklist_id = Nothing
      , Decklist.decklist_name = Nothing
      }

  roundTrip :: Game -> Result Game
  roundTrip = fromJSON . toJSON

  roundTripWithout :: Text -> Game -> Result Game
  roundTripWithout k g = fromJSON $ case toJSON g of
    Object o -> Object (KeyMap.delete (Key.fromText k) o)
    v -> v

  -- Models a two-player campaign start: both seats sit in the shared
  -- deck-selection barrier, `i` has just submitted a deck holding In the Thick
  -- of It, and the AskMap for both seats plus the DoneChoosingDecks
  -- continuation are still queued behind that InitDeck -- which is where the
  -- real campaign start parks them, and what lets the Ask get folded in.
  beginTwoPlayerDeckSelection :: Investigator -> [PlayerId] -> TestAppT ()
  beginTwoPlayerDeckSelection i pids = do
    inTheThickOfIt <- genPlayerCard Assets.inTheThickOfIt
    run
      $ Run
        [ SetGameState (IsChooseDecks pids)
        , InitDeck $ InitDeckAttrs (toId i) Nothing Nothing (Deck [inTheThickOfIt])
        , AskMap (mapFromList [(pid, ChooseDeck) | pid <- pids])
        , DoneChoosingDecks
        ]

  -- Resolves the trauma split for @i@ only if the engine currently has it
  -- parked. Tolerant on purpose: the fix for #5173 may move *when* this choice
  -- is asked, and these specs must not depend on that.
  answerTraumaIfAsked :: Investigator -> Int -> Int -> TestAppT ()
  answerTraumaIfAsked i physical mental = do
    pid <- getPlayer (toId i)
    g <- getGame
    case stripQuestionWrappers <$> lookup pid (gameQuestion g) of
      Just (ChooseAmounts {}) -> replayTraumaAnswer g i physical mental
      _ -> pure ()

  initDeckWith :: Investigator -> TestAppT ()
  initDeckWith i = do
    inTheThickOfIt <- genPlayerCard Assets.inTheThickOfIt
    run $ InitDeck $ InitDeckAttrs (toId i) Nothing Nothing (Deck [inTheThickOfIt])

  answerTrauma :: Investigator -> Int -> Int -> TestAppT ()
  answerTrauma i physical mental = do
    g <- getGame
    replayTraumaAnswer g i physical mental

  -- Builds the AmountsAnswer against the question parked on @g@ but feeds it
  -- through the real server answer path against the CURRENT game, exactly as
  -- the server loads the game fresh for each request. A replay of an already
  -- consumed question must come back Unhandled (and is dropped, as the server
  -- drops it); the trauma assertion catches it if it resolves anyway.
  replayTraumaAnswer :: Game -> Investigator -> Int -> Int -> TestAppT ()
  replayTraumaAnswer g i physical mental = do
    pid <- getPlayer (toId i)
    choices <- case stripQuestionWrappers <$> lookup pid (gameQuestion g) of
      Just (ChooseAmounts _ _ cs _) -> pure cs
      other -> error $ "expected a ChooseAmounts, got: " <> show other
    let
      amountFor lbl = if lbl == "$physical" then physical else mental
      amounts = mapFromList [(c.choiceId, amountFor c.label) | c <- choices]
    current <- getGame
    liftIO (handleAnswerPure current pid (AmountsAnswer (AmountsResponse amounts Nothing (Just pid)))) >>= \case
      Handled msgs -> pushAndRunAll (ClearUI : msgs)
      Unhandled _ -> pure ()
