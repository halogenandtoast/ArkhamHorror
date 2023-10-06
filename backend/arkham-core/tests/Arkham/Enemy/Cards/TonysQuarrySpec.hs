module Arkham.Enemy.Cards.TonysQuarrySpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.Investigator.Cards (tonyMorgan)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token
import TestImport.New

spec :: Spec
spec = describe "Tony's Quarry" do
  context "After Tony's Quarry enters play" do
    isAloof Enemies.tonysQuarry

    it "Place 1 doom on it. Then, place 1 resource on it from the token pool as a bount" . gameTestWith tonyMorgan $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      self `drawsCard` Enemies.tonysQuarry
      useForcedAbility
      tonysQuarry <- selectJust $ enemyIs Enemies.tonysQuarry
      -- spawn at furthest location
      tonysQuarry.location `shouldReturn` Just (toId location2)
      tonysQuarry.doom `shouldReturn` 1
      fieldMap EnemyTokens (countTokens Bounty) tonysQuarry `shouldReturn` 1
