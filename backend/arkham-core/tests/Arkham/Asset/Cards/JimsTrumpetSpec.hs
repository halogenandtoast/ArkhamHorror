module Arkham.Asset.Cards.JimsTrumpetSpec
  ( spec
  ) where

import TestImport

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Investigator.Attrs qualified as Investigator
import Arkham.Investigator.Attrs (Field(..))

spec :: Spec
spec = describe "Jim's Trumpet" $ do
  context "allows you to heal one horror when skull is revealed" $ do
    it "on yourself" $ do
      investigator <- testInvestigator (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset Assets.jimsTrumpet (Just investigator)
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Skull]
          , playAsset investigator jimsTrumpet
          , moveTo investigator location
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((entitiesL . assetsL %~ insertEntity jimsTrumpet)
          . (entitiesL . locationsL %~ insertEntity location)
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
            fieldAssert InvestigatorHorror (== 0) investigator

    it "on an investigator at your location" $ do
      investigator <- testInvestigator id
      investigator2 <- testInvestigator
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- createAsset <$> genPlayerCard Assets.jimsTrumpet
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Skull]
          , playAsset investigator jimsTrumpet
          , moveAllTo location
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((entitiesL . assetsL %~ insertEntity jimsTrumpet)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
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
            fieldAssert InvestigatorHorror (== 0) investigator2

    it "even when another player draws token" $ do
      investigator <- testInvestigator id
      investigator2 <- testInvestigator
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset Assets.jimsTrumpet (Just investigator)
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Skull]
          , playAsset investigator jimsTrumpet
          , moveAllTo location
          , beginSkillTest investigator2 SkillIntellect 0
          ]
          ((entitiesL . assetsL %~ insertEntity jimsTrumpet)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
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
            fieldAssert InvestigatorHorror (== 0) investigator2

    it "on an investigator at a connected location" $ do
      investigator <- testInvestigator id
      investigator2 <- testInvestigator
        (Investigator.sanityDamageL .~ 1)
      jimsTrumpet <- buildAsset Assets.jimsTrumpet (Just investigator)
      rivertown <- createLocation <$> genEncounterCard Locations.rivertown
      southside <- createLocation
        <$> genEncounterCard Locations.southsideHistoricalSociety
      gameTest
          investigator
          [ SetTokens [Skull]
          , placedLocation rivertown
          , placedLocation southside
          , playAsset investigator jimsTrumpet
          , moveTo investigator rivertown
          , moveTo investigator2 southside
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((entitiesL . assetsL %~ insertEntity jimsTrumpet)
          . (entitiesL . locationsL %~ insertEntity rivertown)
          . (entitiesL . locationsL %~ insertEntity southside)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
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
            fieldAssert InvestigatorHorror (== 0) investigator2

    it "cannot target an investigator at an unconnected location" $ do
      investigator <- testInvestigator id
      investigator2 <- testInvestigator
        ((Investigator.sanityDamageL .~ 1) . (Investigator.idL .~ "01001"))
      jimsTrumpet <- buildAsset Assets.jimsTrumpet (Just investigator)
      rivertown <- createLocation <$> genEncounterCard Locations.rivertown
      downtown <- createLocation <$> genEncounterCard Locations.downtownArkhamAsylum
      gameTest
          investigator
          [ SetTokens [Skull]
          , placedLocation rivertown
          , placedLocation downtown
          , playAsset investigator jimsTrumpet
          , moveTo investigator rivertown
          , moveTo investigator2 downtown
          , beginSkillTest investigator SkillIntellect 0
          ]
          ((entitiesL . assetsL %~ insertEntity jimsTrumpet)
          . (entitiesL . locationsL %~ insertEntity rivertown)
          . (entitiesL . locationsL %~ insertEntity downtown)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            fieldAssert InvestigatorHorror (== 1) investigator2
