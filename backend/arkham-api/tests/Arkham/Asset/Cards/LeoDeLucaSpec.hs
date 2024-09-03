module Arkham.Asset.Cards.LeoDeLucaSpec (spec) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Leo De Luca" $ do
  it "gives +1 action" . gameTest $ \self -> do
    self `putCardIntoPlay` Assets.leoDeLuca
    self.additionalActions `shouldSatisfyM` ((== 1) . length)

  faq "additions actions are used first" $ do
    it "when discarded doesn't lose you an action" . gameTest $ \self -> do
      leoDeLuca <- self `putAssetIntoPlay` Assets.leoDeLuca
      duringRound $ do
        duringTurn self $ do
          takeResource self
          self.remainingActions `shouldReturn` 3
          discard leoDeLuca
          self.remainingActions `shouldReturn` 3

    it "loses the action if discarded before taking an action" . gameTest $ \self -> do
      leoDeLuca <- self `putAssetIntoPlay` Assets.leoDeLuca
      duringRound $ do
        duringTurn self $ do
          self.additionalActions `shouldSatisfyM` notNull
          discard leoDeLuca
          self.additionalActions `shouldSatisfyM` null
          self.remainingActions `shouldReturn` 3

  faq "If you take an action that qualifies as either of your additional actions" $ do
    it "you may choose which additional action you are actually using." . gameTest $ \self -> do
      leoDeLuca <- self `putAssetIntoPlay` Assets.leoDeLuca
      theBlackFan3 <- self `putAssetIntoPlay` Assets.theBlackFan3
      self `gainResources` 15
      location <- testLocation
      self `moveTo` location
      withEach [leoDeLuca, theBlackFan3] $ \choice -> do
        duringRound $ do
          duringTurn self $ do
            [chosenAction] <- filter ((== toSource choice) . additionalActionSource) <$> self.additionalActions
            takeResource self
            chooseOptionMatching "use action from choice" $ \case
              Label _ (LoseAdditionalAction _ additionalAction : _) -> additionalAction == chosenAction
              _ -> False
            self.additionalActions `shouldSatisfyM` none ((== toSource choice) . additionalActionSource)
            self.additionalActions `shouldSatisfyM` notNull
