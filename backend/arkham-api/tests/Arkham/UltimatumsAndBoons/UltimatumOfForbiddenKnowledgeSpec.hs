module Arkham.UltimatumsAndBoons.UltimatumOfForbiddenKnowledgeSpec (spec) where

import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of Forbidden Knowledge" $ do
  it "each investigator draws 1 fewer card in their opening hand" . gameTest $ \self -> do
    withUltimatums [UltimatumOfForbiddenKnowledge]
    cards <- testPlayerCards 10
    withProp @"deck" (Deck cards) self
    run $ DrawStartingHand (toId self)
    self.hand `shouldSatisfyM` ((== 4) . length)

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
    withUltimatumsDisabled [UltimatumOfForbiddenKnowledge]
    cards <- testPlayerCards 10
    withProp @"deck" (Deck cards) self
    run $ DrawStartingHand (toId self)
    self.hand `shouldSatisfyM` ((== 5) . length)
