module Arkham.UltimatumsAndBoons.BoonOfDestinySpec (spec) where

import Arkham.Event.Cards qualified as Events
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of Destiny" $ do
  it "searches your deck for a card before drawing your opening hand (once per game)" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfDestiny]
    otherCards <- testPlayerCards 14
    emergencyCache <- genPlayerCard Events.emergencyCache
    withProp @"deck" (Deck (emergencyCache : otherCards)) self
    run DrawStartingHands
    useReactionOf (UltimatumOrBoonSource (Boon BoonOfDestiny))
    chooseTarget emergencyCache
    -- the found card is added to hand and the opening hand still draws 5
    self.hand `shouldSatisfyM` elem (toCard emergencyCache)
    self.hand `shouldSatisfyM` ((== 6) . length)
    -- once per game: a second opening hand offers no reaction and just draws
    run DrawStartingHands
    self.hand `shouldSatisfyM` ((== 11) . length)
