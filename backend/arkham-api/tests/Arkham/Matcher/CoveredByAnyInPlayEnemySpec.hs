{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Arkham.Matcher.CoveredByAnyInPlayEnemySpec (spec) where

import Arkham.Matcher.Enemy
import Arkham.Matcher.Investigator (InvestigatorMatcher (InvestigatorWithId))
import Arkham.Matcher.Patterns (pattern AnyInPlayEnemy)
import Arkham.Prelude
import Test.Hspec

spec :: Spec
spec = describe "coveredByAnyInPlayEnemy" $ do
  it "is True for any in-play enemy" $
    coveredByAnyInPlayEnemy AnyInPlayEnemy `shouldBe` True

  it "is True when one branch of a OneOf covers any in-play enemy" $
    coveredByAnyInPlayEnemy
      ( EnemyOneOf
          [ AnyInPlayEnemy
          , EnemyHiddenInHand (InvestigatorWithId "60101")
          ]
      )
      `shouldBe` True

  it "is False when an additional restriction is required (e.g. EnemyCanAttack)" $
    coveredByAnyInPlayEnemy
      ( EnemyMatchAll
          [ EnemyOneOf
              [ AnyInPlayEnemy
              , EnemyHiddenInHand (InvestigatorWithId "60101")
              ]
          , EnemyCanAttack (InvestigatorWithId "60101")
          ]
      )
      `shouldBe` False
