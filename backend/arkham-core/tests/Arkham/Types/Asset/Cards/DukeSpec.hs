module Arkham.Types.Asset.Cards.DukeSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token
import Arkham.Types.Window

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage" $ do
      duke <- buildAsset "02014"
      enemy <- testEnemy ((Enemy.health .~ Static 3) . (Enemy.fight .~ 4))
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorCombat = 1 }
      location <- testLocation "00000" id
      scenario' <- testScenario "00000" id
      game <- runGameTest
        investigator
        [SetTokens [Zero], playAsset investigator duke]
        ((enemies %~ insertEntity enemy)
        . (locations %~ insertEntity location)
        . (scenario ?~ scenario')
        . (assets %~ insertEntity duke)
        )
      let dukeAsset = game ^?! assets . to toList . ix 0
      (fightAction : _) <- toInternalGame game
        >>= runReaderT (getActions investigator NonFast dukeAsset)
      game' <-
        runGameTestMessages
          game
          [enemySpawn location enemy, moveTo investigator location, fightAction]
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Start skill test"
        >>= runGameTestOnlyOption "Apply Results"
      updated game' enemy `shouldSatisfy` hasDamage (2, 0)
  context "investigate action" $ do
    it "uses a base intellect skill of 4" pending
    it
      "you may move to a connecting location immediately before investigating"
      pending
