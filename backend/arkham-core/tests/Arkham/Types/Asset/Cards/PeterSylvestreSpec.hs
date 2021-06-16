module Arkham.Types.Asset.Cards.PeterSylvestreSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Modifier

spec :: Spec
spec = describe "Peter Sylvestre" $ do
  it "gives you +1 agility" $ do
    investigator <- testInvestigator "00000" id
    peterSylvestre <- buildAsset "02033"
    gameTest
        investigator
        [playAsset investigator peterSylvestre]
        (assetsL %~ insertEntity peterSylvestre)
      $ do
          runMessages
          (map modifierType
            <$> getModifiersFor (TestSource mempty) (toTarget investigator) ()
            )
            `shouldReturn` [SkillModifier SkillAgility 1]

  it "removes one horror at the end of your turn" $ do
    investigator <- testInvestigator "00000" id
    peterSylvestre <- buildAsset "02033"
    gameTest
        investigator
        [ playAsset investigator peterSylvestre
        , AssetDamage (toId peterSylvestre) (TestSource mempty) 0 2
        , ChooseEndTurn (toId investigator)
        ]
        (assetsL %~ insertEntity peterSylvestre)
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          updated peterSylvestre `shouldSatisfyM` hasDamage (0, 1)
