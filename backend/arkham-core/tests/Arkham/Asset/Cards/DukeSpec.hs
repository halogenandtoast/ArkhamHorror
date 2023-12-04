module Arkham.Asset.Cards.DukeSpec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Location.Types (LocationAttrs (..))
import Arkham.Matcher (assetIs)
import Arkham.Projection
import Arkham.Token

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage"
      $ gameTest
      $ \investigator -> do
        enemy <- testEnemyWith ((Enemy.healthL ?~ Static 3) . (Enemy.fightL ?~ 4))
        updateInvestigator investigator
          $ \attrs -> attrs {investigatorCombat = 1}
        putCardIntoPlay investigator Assets.duke
        duke <- selectJust $ assetIs Assets.duke
        location <- testLocationWith id
        pushAndRun $ SetChaosTokens [Zero]
        pushAndRun $ spawnAt enemy location
        pushAndRun $ moveTo investigator location
        [doFight, _] <- field AssetAbilities duke
        pushAndRun $ UseAbility (toId investigator) doFight []
        chooseOnlyOption "Fight enemy"
        chooseOnlyOption "Start skill test"
        chooseOnlyOption "Apply Results"
        fieldAssert EnemyDamage (== 2) enemy

  context "investigate action" $ do
    it "uses a base intellect skill of 4"
      $ gameTest
      $ \investigator -> do
        updateInvestigator investigator
          $ \attrs -> attrs {investigatorIntellect = 1}
        putCardIntoPlay investigator Assets.duke
        duke <- selectJust $ assetIs Assets.duke
        location <-
          testLocationWith
            (\attrs -> attrs {locationShroud = 4, locationTokens = setTokens Clue 1 mempty})
        pushAndRun $ SetChaosTokens [Zero]
        pushAndRun $ moveTo investigator location
        [_, investigateAction] <- field AssetAbilities duke
        pushAndRun
          $ UseAbility
            (toId investigator)
            investigateAction
            [duringTurn $ toId investigator]
        chooseOnlyOption "Investigate current location"
        chooseOnlyOption "Start skill test"
        chooseOnlyOption "Apply results"
        fieldAssert InvestigatorClues (== 1) investigator

    it "you may move to a connecting location immediately before investigating"
      $ gameTest
      $ \investigator -> do
        updateInvestigator investigator
          $ \attrs -> attrs {investigatorIntellect = 1}
        putCardIntoPlay investigator Assets.duke
        duke <- selectJust $ assetIs Assets.duke
        (location1, location2) <- testConnectedLocations id
          $ \attrs -> attrs {locationShroud = 4, locationTokens = setTokens Clue 1 mempty}
        pushAndRun $ placedLocation location1
        pushAndRun $ placedLocation location2
        pushAndRun $ SetChaosTokens [Zero]
        pushAndRun $ moveTo investigator location1
        [_, investigateAction] <- field AssetAbilities duke
        pushAndRun $ moveTo investigator location1
        pushAndRun $ UseAbility (toId investigator) investigateAction []
        chooseOptionMatching
          "move first"
          ( \case
              TargetLabel {} -> True
              _ -> False
          )
        chooseOnlyOption "Start skill test"
        chooseOnlyOption "Apply results"
        fieldAssert InvestigatorClues (== 1) investigator
