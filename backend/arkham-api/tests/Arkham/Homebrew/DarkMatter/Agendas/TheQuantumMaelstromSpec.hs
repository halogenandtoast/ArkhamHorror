module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstromSpec (spec) where

import Arkham.Helpers.Scenario (getAgendaDeckCards)
import Arkham.Matcher
import TestImport.New

{- | Lost Quantum shuffles all three printings of agenda 1 together to form its
agenda deck, and each advance "sets this agenda aside, out of play" until none
are left below it, at which point the set aside printings are shuffled back
into a new agenda deck.
-}
spec :: Spec
spec = describe "The Quantum Maelstrom" do
  it "starts with all three printings of agenda 1 in the agenda deck"
    . scenarioTest ":dark-matter:089"
    $ \_ -> do
      pushAndRun Setup
      deck <- getAgendaDeckCards 1
      liftIO $ length deck `shouldBe` 3

  it "sets each printing aside as it advances, then reforms the deck"
    . scenarioTest ":dark-matter:089"
    $ \_ -> do
      pushAndRun Setup
      -- Setup pauses on its instructions; keep only the message that puts the
      -- top agenda into play so the test never reaches the first turn.
      queued <- peekQueue
      setQueue $ filter (== SetAgendaDeck) queued
      overTest (questionL .~ mempty)
      runMessages

      for_ [2, 1] \remaining -> do
        advanceCurrentAgenda
        deck <- getAgendaDeckCards 1
        liftIO $ length deck `shouldBe` remaining

      -- The last printing has nothing below it, so it shuffles itself back
      -- together with the two set aside printings.
      advanceCurrentAgenda
      deck <- getAgendaDeckCards 1
      liftIO $ length deck `shouldBe` 3
 where
  advanceCurrentAgenda = do
    agenda <- selectJust AnyAgenda
    run $ AdvanceAgendaBy agenda AgendaAdvancedWithOther
    chooseOnlyOption "advance agenda"
