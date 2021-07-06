module Arkham.Types.Asset.Cards.JimsTrumpetSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Investigator.Attrs as Investigator

spec :: Spec
spec = describe "Jim's Trumpet" $ do
  context "allows you to heal one horror when skull is revealed" $ do
    it "on yourself" $ do
      investigator <- testInvestigator "00000" (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Skull]
          , playAsset investigator jimsTrumpet
          , moveTo investigator location
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assetsL %~ insertEntity jimsTrumpet)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "choose self"
            updated investigator `shouldSatisfyM` hasDamage (0, 0)

    it "on an investigator at your location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator
        "00001"
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Skull]
          , playAsset investigator jimsTrumpet
          , moveAllTo location
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assetsL %~ insertEntity jimsTrumpet)
          . (locationsL %~ insertEntity location)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "choose investigator at same location"
            updated investigator2 `shouldSatisfyM` hasDamage (0, 0)

    it "even when another player draws token" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator
        "00001"
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Skull]
          , playAsset investigator jimsTrumpet
          , moveAllTo location
          , beginSkillTest investigator2 SkillIntellect 0
          ]
          ((assetsL %~ insertEntity jimsTrumpet)
          . (locationsL %~ insertEntity location)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "choose investigator at same location"
            updated investigator2 `shouldSatisfyM` hasDamage (0, 0)

    it "on an investigator at a connected location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator
        "00001"
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset "02012"
      (location1, location2) <- testConnectedLocations id id
      gameTest
          investigator
          [ SetTokens [Skull]
          , placedLocation location1
          , placedLocation location2
          , playAsset investigator jimsTrumpet
          , moveTo investigator location1
          , moveTo investigator2 location2
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assetsL %~ insertEntity jimsTrumpet)
          . (locationsL %~ insertEntity location1)
          . (locationsL %~ insertEntity location2)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "choose investigator at connected location"
            updated investigator2 `shouldSatisfyM` hasDamage (0, 0)

    it "cannot target an investigator at an unconnected location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator
        "00001"
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset "02012"
      (location1, location2) <- testUnconnectedLocations id id
      gameTest
          investigator
          [ SetTokens [Skull]
          , placedLocation location1
          , placedLocation location2
          , playAsset investigator jimsTrumpet
          , moveTo investigator location1
          , moveTo investigator2 location2
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assetsL %~ insertEntity jimsTrumpet)
          . (locationsL %~ insertEntity location1)
          . (locationsL %~ insertEntity location2)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            updated investigator2 `shouldSatisfyM` hasDamage (0, 1)
