module Arkham.Types.Event.Cards.EmergencyCacheSpec where

import Arkham.Types.Event
import Arkham.Types.EventId
import Arkham.Types.Investigator
import Data.UUID.V4
import TestImport

spec :: Spec
spec = do
  describe "Emegency cache" $ do

    let
      investigatorId = "01001"
      investigator = lookupInvestigator investigatorId

    it "should increase the investigators resources by 3" $ do
      eventId <- EventId <$> nextRandom
      queue <- newIORef [InvestigatorPlayEvent investigatorId eventId]
      let
        event = lookupEvent "01088" investigatorId eventId
        game = newGame investigator queue
      g' <- runMessages $ game & events %~ insertMap eventId event
      g'
        ^?! investigators
        . ix investigatorId
        . to (resourceCount . investigatorAttrs)
        `shouldBe` 3
