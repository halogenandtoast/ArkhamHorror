{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Arkham.Matcher.CoveredByAnyInPlayEnemySpec (spec) where

import Arkham.Criteria (
  canFightIgnoreAloof,
  fightOffersAsIfEnemyTargets,
  fightOverride,
  ignoreAloofFightOverride,
 )
import Arkham.Criteria.Override (CriteriaOverride (..))
import Arkham.Id (AssetId (..), EnemyId (..))
import Arkham.Matcher.Asset (AssetMatcher (AssetWithId))
import Arkham.Matcher.Enemy
import Arkham.Matcher.Investigator (InvestigatorMatcher (InvestigatorWithId))
import Arkham.Matcher.Location (LocationMatcher (LocationWithAsset))
import Arkham.Matcher.Patterns (pattern AnyInPlayEnemy)
import Arkham.Prelude
import Data.UUID (nil)
import Test.Hspec

spec :: Spec
spec = do
  describe "coveredByAnyInPlayEnemy" $ do
    it "is True for any in-play enemy"
      $ coveredByAnyInPlayEnemy AnyInPlayEnemy
      `shouldBe` True

    it "is True when one branch of a OneOf covers any in-play enemy"
      $ coveredByAnyInPlayEnemy
        ( EnemyOneOf
            [ AnyInPlayEnemy
            , EnemyHiddenInHand (InvestigatorWithId "60101")
            ]
        )
      `shouldBe` True

    it "is False when an additional restriction is required (e.g. EnemyCanAttack)"
      $ coveredByAnyInPlayEnemy
        ( EnemyMatchAll
            [ EnemyOneOf
                [ AnyInPlayEnemy
                , EnemyHiddenInHand (InvestigatorWithId "60101")
                ]
            , EnemyCanAttack (InvestigatorWithId "60101")
            ]
        )
      `shouldBe` False

  describe "fightOffersAsIfEnemyTargets" $ do
    it "is True for an unrestricted fight"
      $ fightOffersAsIfEnemyTargets AnyInPlayEnemy
      `shouldBe` True

    it "is True for Longbow (3)'s ignore-Aloof override"
      $ fightOffersAsIfEnemyTargets (ignoreAloofFightOverride AnyEnemy)
      `shouldBe` True

    it "is True for British Bull Dog (2)'s ignore-Aloof criteria override"
      $ fightOffersAsIfEnemyTargets
        (CanFightEnemyWithOverride $ CriteriaOverride canFightIgnoreAloof)
      `shouldBe` True

    it "is False for an override narrowed to one enemy (Service Revolver)"
      $ fightOffersAsIfEnemyTargets (fightOverride $ EnemyWithId (EnemyId nil))
      `shouldBe` False

    it "is False for an override narrowed to another location (Summoned Servitor)"
      $ fightOffersAsIfEnemyTargets
        (fightOverride $ EnemyAt $ LocationWithAsset $ AssetWithId (AssetId nil))
      `shouldBe` False

    it "is False for a matcher narrowed without an override (Toe to Toe)"
      $ fightOffersAsIfEnemyTargets
        (EnemyMatchAll [AnyInPlayEnemy, EnemyCanAttack (InvestigatorWithId "60101")])
      `shouldBe` False
