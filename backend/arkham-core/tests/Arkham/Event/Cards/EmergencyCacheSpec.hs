module Arkham.Event.Cards.EmergencyCacheSpec where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field (..))

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" $ gameTest $ \investigator -> do
      putCardIntoPlay investigator Events.emergencyCache
      fieldAssert InvestigatorResources (== 3) investigator
