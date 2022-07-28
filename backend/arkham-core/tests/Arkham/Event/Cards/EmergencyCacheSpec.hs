module Arkham.Event.Cards.EmergencyCacheSpec where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field(..))

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" $ do
      investigator <- testJenny id
      emergencyCache <- buildEvent Events.emergencyCache investigator
      gameTest
          investigator
          [playEvent investigator emergencyCache]
          (entitiesL . eventsL %~ insertEntity emergencyCache)
        $ do
            runMessages
            fieldAssert InvestigatorResources (== 3) investigator
