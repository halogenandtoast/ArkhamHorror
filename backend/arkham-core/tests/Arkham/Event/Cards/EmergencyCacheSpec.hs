module Arkham.Event.Cards.EmergencyCacheSpec where

import TestImport.Lifted

import Arkham.Investigator.Attrs (Field(..))

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" $ do
      investigator <- testJenny id
      emergencyCache <- buildEvent "01088" investigator
      gameTest
          investigator
          [playEvent investigator emergencyCache]
          (entitiesL . eventsL %~ insertEntity emergencyCache)
        $ do
            runMessages
            fieldAssert InvestigatorResources (== 3) investigator
