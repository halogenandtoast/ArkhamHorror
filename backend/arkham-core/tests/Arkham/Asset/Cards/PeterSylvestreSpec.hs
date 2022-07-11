module Arkham.Asset.Cards.PeterSylvestreSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Projection
import Arkham.Asset.Attrs qualified as Asset

spec :: Spec
spec = describe "Peter Sylvestre" $ do
  it "gives you +1 agility" $ do
    investigator <- testInvestigator id
    peterSylvestre <- buildAsset "02033" (Just investigator)
    gameTest
        investigator
        [playAsset investigator peterSylvestre]
        (entitiesL . assetsL %~ insertEntity peterSylvestre)
      $ do
          runMessages
          getModifiers (TestSource mempty) (toTarget investigator)
            `shouldReturn` [SkillModifier SkillAgility 1]

  it "removes one horror at the end of your turn" $ do
    investigator <- testInvestigator id
    peterSylvestre <- buildAsset "02033" (Just investigator)
    gameTest
        investigator
        [ playAsset investigator peterSylvestre
        , AssetDamage (toId peterSylvestre) (TestSource mempty) 0 2
        , ChooseEndTurn (toId investigator)
        ]
        (entitiesL . assetsL %~ insertEntity peterSylvestre)
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          assert $ fieldP Asset.AssetHorror (== 1) (toId peterSylvestre)
