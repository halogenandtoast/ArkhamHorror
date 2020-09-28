module Arkham.Types.Asset.Cards.JimsTrumpetSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Helpers
import qualified Arkham.Types.Investigator.Attrs as Investigator
import qualified Arkham.Types.Location.Attrs as Location
import Arkham.Types.LocationSymbol
import Arkham.Types.Token

spec :: Spec
spec = describe "Jim's Trumpet" $ do
  context "allows you to heal one horror when skull is revealed" $ do
    it "on yourself" $ do
      investigator <- testInvestigator "00000" (Investigator.sanityDamage .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location <- testLocation "00000" id
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          investigator
          [ playAsset investigator jimsTrumpet
          , moveTo investigator location
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assets %~ insertEntity jimsTrumpet)
          . (chaosBag .~ Bag [Skull])
          . (scenario ?~ scenario')
          . (locations %~ insertEntity location)
          )
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "choose self"
      updated game investigator `shouldSatisfy` hasDamage (0, 0)

    it "on an investigator at your location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" (Investigator.sanityDamage .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location <- testLocation "00000" id
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          investigator
          [ playAsset investigator jimsTrumpet
          , moveAllTo location
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assets %~ insertEntity jimsTrumpet)
          . (chaosBag .~ Bag [Skull])
          . (scenario ?~ scenario')
          . (locations %~ insertEntity location)
          . (investigators %~ insertEntity investigator2)
          )
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "choose investigator at same location"
      updated game investigator2 `shouldSatisfy` hasDamage (0, 0)

    it "even when another player draws token" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" (Investigator.sanityDamage .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location <- testLocation "00000" id
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          investigator
          [ playAsset investigator jimsTrumpet
          , moveAllTo location
          , beginSkillTest investigator2 SkillIntellect 0
          ]
          ((assets %~ insertEntity jimsTrumpet)
          . (chaosBag .~ Bag [Skull])
          . (scenario ?~ scenario')
          . (locations %~ insertEntity location)
          . (investigators %~ insertEntity investigator2)
          )
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "choose investigator at same location"
      updated game investigator2 `shouldSatisfy` hasDamage (0, 0)

    it "on an investigator at a connected location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" (Investigator.sanityDamage .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location1 <- testLocation
        "00000"
        ((Location.symbol .~ Square)
        . (Location.revealedSymbol .~ Square)
        . (Location.connectedSymbols .~ setFromList [Triangle])
        . (Location.revealedConnectedSymbols .~ setFromList [Triangle])
        )
      location2 <- testLocation
        "00001"
        ((Location.symbol .~ Triangle)
        . (Location.revealedSymbol .~ Triangle)
        . (Location.connectedSymbols .~ setFromList [Square])
        . (Location.revealedConnectedSymbols .~ setFromList [Square])
        )
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          investigator
          [ PlacedLocation (getId () location1)
          , PlacedLocation (getId () location2)
          , playAsset investigator jimsTrumpet
          , moveTo investigator location1
          , moveTo investigator2 location2
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assets %~ insertEntity jimsTrumpet)
          . (chaosBag .~ Bag [Skull])
          . (scenario ?~ scenario')
          . (locations %~ insertEntity location1)
          . (locations %~ insertEntity location2)
          . (investigators %~ insertEntity investigator2)
          )
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "choose investigator at connected location"
      updated game investigator2 `shouldSatisfy` hasDamage (0, 0)

    it "cannot target an investigator at an unconnected location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" (Investigator.sanityDamage .~ 1)
      jimsTrumpet <- buildAsset "02012"
      location1 <- testLocation
        "00000"
        ((Location.symbol .~ Square) . (Location.revealedSymbol .~ Square))
      location2 <- testLocation
        "00001"
        ((Location.symbol .~ Triangle) . (Location.revealedSymbol .~ Triangle))
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          investigator
          [ PlacedLocation (getId () location1)
          , PlacedLocation (getId () location2)
          , playAsset investigator jimsTrumpet
          , moveTo investigator location1
          , moveTo investigator2 location2
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((assets %~ insertEntity jimsTrumpet)
          . (chaosBag .~ Bag [Skull])
          . (scenario ?~ scenario')
          . (locations %~ insertEntity location1)
          . (locations %~ insertEntity location2)
          . (investigators %~ insertEntity investigator2)
          )
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      updated game investigator2 `shouldSatisfy` hasDamage (0, 1)
