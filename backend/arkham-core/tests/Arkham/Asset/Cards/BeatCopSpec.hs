{-# OPTIONS_GHC -Wno-deprecations #-}
module Arkham.Asset.Cards.BeatCopSpec
  ( spec
  )
where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Attrs (Field(..), InvestigatorAttrs(..))

spec :: Spec
spec = describe "Beat Cop" $ do
  it "gives you +1 combat" $ do
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorCombat = 1 }
    beatCop <- buildAsset Assets.beatCop (Just investigator)
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , playAsset investigator beatCop
        , moveTo investigator location
        ]
        ((entitiesL . assetsL %~ insertEntity beatCop)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          pending
          fieldAssert InvestigatorCombat (== 2) investigator

  it "can be discarded to do 1 damage to an enemy at your location" $ do
    investigator <- testInvestigator id
    gameTest investigator [] id pending
