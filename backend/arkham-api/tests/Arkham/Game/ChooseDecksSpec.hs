module Arkham.Game.ChooseDecksSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Game.State
import Arkham.Projection (field)
import Arkham.Question
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
 where
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
