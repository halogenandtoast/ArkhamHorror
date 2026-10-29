module Arkham.Log.NarratorSpec (spec) where

import Arkham.Deck qualified as Deck
import Arkham.Log
import Arkham.Log.Narrator (narrationFor)
import Data.Map.Strict qualified as Map
import TestImport.New

narrate :: Message -> TestAppT LogEntry
narrate message = case narrationFor message of
  Nothing -> error "Expected a narrator handler"
  Just renderEntry -> renderEntry >>= maybe (error "Expected a narrated entry") pure

assertCards :: Text -> Card -> LogEntry -> TestAppT ()
assertCards key card entry = case logEntryBody entry of
  [LogI18n actual vars] -> do
    actual `shouldBe` key
    Map.lookup "cards" vars `shouldBe` Just (LogList [toLogPart card])
  _ -> expectationFailure "Expected a localized card movement entry"

spec :: Spec
spec = describe "card movement narration" do
  it "names the exiled card from the completed exile message" . gameTest $ \_ -> do
    [card] <- testPlayerCards 1
    entry <- narrate $ Exiled (CardIdTarget card.id) (toCard card)
    logEntryBody entry `shouldBe` [ikeyPart "log.exiled" ["card" ~> toLogPart (toCard card)]]
    logEntryAudience entry `shouldBe` Everyone
    case (narrationFor (Exile (CardIdTarget card.id)) :: Maybe (TestAppT (Maybe LogEntry))) of
      Nothing -> pure ()
      Just _ -> expectationFailure "Exile requests should not duplicate the completed exile entry"
  it "names shuffled cards and discard returns for their owner without treating a return as a draw" . gameTest $ \self -> do
    [shuffled, returned] <- testPlayerCards 2
    withProp @"hand" [toCard shuffled] self
    withProp @"discard" [returned] self
    pid <- getPlayer self.id
    shuffleEntry <- narrate $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck self.id) [toCard shuffled]
    assertCards "log.shufflesCardsIntoDeck" (toCard shuffled) shuffleEntry
    logEntryAudience shuffleEntry `shouldBe` OnlyPlayer pid
    returnEntry <- narrate $ AddToHand self.id [toCard returned]
    assertCards "log.returnsCardsFromDiscardToHand" (toCard returned) returnEntry
    logEntryAudience returnEntry `shouldBe` OnlyPlayer pid
    run $ AddToHand self.id [toCard returned]
    case narrationFor (AddToHand self.id [toCard returned]) of
      Nothing -> expectationFailure "Expected the AddToHand handler"
      Just renderEntry -> renderEntry `shouldReturn` Nothing
