module Arkham.Asset.Assets.RunicAxeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (AssetAttrs (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Placement
import Arkham.Projection
import Arkham.Token
import Data.IntMap.Strict qualified as IntMap
import TestImport.New

spec :: Spec
spec = describe "Runic Axe" $ do
  it "pays an enemy's additional engage cost when using Inscription of the Hunt" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    run $ PlaceTokens (TestSource mempty) (toTarget self) Clue 1

    axe <-
      testAssetWithDef
        Assets.runicAxe
        ( \attrs ->
            attrs
              { assetPlacement = InPlayArea (toId self)
              , assetCustomizations = IntMap.singleton 3 (1, [])
              , assetTokens = singletonMap Charge 4
              }
        )
        self
    rougarou <- testEnemyWithDef Enemies.theRougarou id
    rougarou `spawnAt` location

    let fight = fromJustNote "Runic Axe fight ability" $ listToMaybe $ getAbilities axe
    self `useAbility` fight
    chooseOptionMatching "choose The Rougarou" $ \case
      FightLabel {enemyId} -> enemyId == toId rougarou
      _ -> False

    self.clues `shouldReturn` 0
    field EnemyPlacement rougarou.id `shouldReturn` InThreatArea (toId self)
