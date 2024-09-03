module Arkham.Asset.Cards.ForbiddenKnowledgeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Forbidden Knowledge" $ do
  hasUses @"secret" Assets.forbiddenKnowledge 4
  discardedWhenNoUses Assets.forbiddenKnowledge
  it "exhausts, take 1 horror, move 1 secret as a resource" . gameTest $ \self -> do
    withProp @"resources" 0 self
    forbiddenKnowledge <- self `putAssetIntoPlay` Assets.forbiddenKnowledge
    [action] <- self `getActionsFrom` forbiddenKnowledge
    self `useAbility` action
    applyAllHorror
    self.horror `shouldReturn` 1
    self.resources `shouldReturn` 1
    forbiddenKnowledge.secrets `shouldReturn` 3
    assert forbiddenKnowledge.exhausted
