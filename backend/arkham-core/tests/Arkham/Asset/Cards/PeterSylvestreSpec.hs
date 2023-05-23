module Arkham.Asset.Cards.PeterSylvestreSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types qualified as Asset
import Arkham.Matcher (assetIs)
import Arkham.Projection

spec :: Spec
spec = describe "Peter Sylvestre" $ do
  it "gives you +1 agility" $ gameTest $ \investigator -> do
    putCardIntoPlay investigator Assets.peterSylvestre
    getModifiers (toTarget investigator)
      `shouldReturn` [SkillModifier SkillAgility 1]

  it "removes one horror at the end of your turn" $ gameTest $ \investigator -> do
    putCardIntoPlay investigator Assets.peterSylvestre
    peterSylvestre <- selectJust $ assetIs Assets.peterSylvestre
    pushAndRun $ AssetDamage peterSylvestre (TestSource mempty) 0 2
    pushAndRun $ ChooseEndTurn (toId investigator)
    chooseOptionMatching
      "use ability"
      ( \case
          AbilityLabel {} -> True
          _ -> False
      )
    assert $ fieldP Asset.AssetHorror (== 1) peterSylvestre
