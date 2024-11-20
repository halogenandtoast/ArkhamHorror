{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.SkillTest where

import Arkham.Matcher.Action
import Arkham.Matcher.Asset
import Arkham.Matcher.Card
import Arkham.Matcher.ChaosToken
import Arkham.Matcher.Enemy
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Skill
import Arkham.Matcher.Source
import Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Trait (Trait)
import Data.Aeson.TH
import GHC.OverloadedLabels

data SkillTestMatcher
  = WhileInvestigating LocationMatcher
  | WhileAttackingAnEnemy EnemyMatcher
  | WhileEvadingAnEnemy EnemyMatcher
  | WhileParleyingWithAnEnemy EnemyMatcher
  | WhileParleying
  | SkillTestWithAction ActionMatcher
  | SkillTestWithSkill SkillMatcher
  | SkillTestWithSkillType SkillType
  | AnySkillTest
  | SkillTestWasFailed
  | YourSkillTest SkillTestMatcher
  | SkillTestAtYourLocation
  | SkillTestAt LocationMatcher
  | SkillTestOfInvestigator InvestigatorMatcher
  | SkillTestOnTreachery TreacheryMatcher
  | SkillTestOnLocation LocationMatcher
  | SkillTestOnAsset AssetMatcher
  | UsingThis
  | SkillTestOnEncounterCard
  | SkillTestSourceMatches SourceMatcher
  | SkillTestMatches [SkillTestMatcher]
  | SkillTestOneOf [SkillTestMatcher]
  | NotSkillTest SkillTestMatcher
  | SkillTestFromRevelation
  | SkillTestWithRevealedChaosToken ChaosTokenMatcher
  | SkillTestWithRevealedChaosTokenCount Int ChaosTokenMatcher
  | SkillTestWithResolvedChaosTokenBy InvestigatorMatcher ChaosTokenMatcher
  | SkillTestOnCardWithTrait Trait
  | SkillTestOnCard CardMatcher
  | SkillTestWithDifficulty ValueMatcher
  | PerilousSkillTest
  | IfSkillTestMatcher SkillTestMatcher SkillTestMatcher SkillTestMatcher
  | SkillTestBeforeRevealingChaosTokens
  deriving stock (Show, Eq, Ord, Data, Generic)

instance IsLabel "willpower" SkillTestMatcher where
  fromLabel = SkillTestWithSkillType #willpower

instance IsLabel "intellect" SkillTestMatcher where
  fromLabel = SkillTestWithSkillType #intellect

instance IsLabel "combat" SkillTestMatcher where
  fromLabel = SkillTestWithSkillType #combat

instance IsLabel "agility" SkillTestMatcher where
  fromLabel = SkillTestWithSkillType #agility

instance IsLabel "investigating" SkillTestMatcher where
  fromLabel = WhileInvestigating Anywhere

instance IsLabel "investigation" SkillTestMatcher where
  fromLabel = WhileInvestigating Anywhere

instance IsLabel "parley" SkillTestMatcher where
  fromLabel = WhileParleying

instance IsLabel "parleying" SkillTestMatcher where
  fromLabel = WhileParleying

instance IsLabel "fighting" SkillTestMatcher where
  fromLabel = WhileAttackingAnEnemy AnyEnemy

instance IsLabel "attacking" SkillTestMatcher where
  fromLabel = WhileAttackingAnEnemy AnyEnemy

instance IsLabel "evading" SkillTestMatcher where
  fromLabel = WhileEvadingAnEnemy AnyEnemy

instance IsLabel "any" SkillTestMatcher where
  fromLabel = AnySkillTest

instance IsLabel "failed" SkillTestMatcher where
  fromLabel = SkillTestWasFailed

instance Semigroup SkillTestMatcher where
  AnySkillTest <> x = x
  x <> AnySkillTest = x
  SkillTestMatches xs <> SkillTestMatches ys = SkillTestMatches $ xs <> ys
  SkillTestMatches xs <> x = SkillTestMatches $ xs <> [x]
  x <> SkillTestMatches xs = SkillTestMatches $ x : xs
  x <> y = SkillTestMatches [x, y]

instance Monoid SkillTestMatcher where
  mempty = AnySkillTest

data SkillTestResultMatcher
  = FailureResult ValueMatcher
  | SuccessResult ValueMatcher
  | AnyResult
  | ResultOneOf [SkillTestResultMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "success" SkillTestResultMatcher where
  fromLabel = SuccessResult AnyValue

instance IsLabel "failure" SkillTestResultMatcher where
  fromLabel = FailureResult AnyValue

instance IsLabel "any" SkillTestResultMatcher where
  fromLabel = AnyResult

data SkillTestValueMatcher
  = SkillTestGameValue ValueMatcher
  | GreaterThanBaseValue
  | AnySkillTestValue
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" SkillTestValueMatcher where
  fromLabel = AnySkillTestValue

mconcat
  [ deriveToJSON defaultOptions ''SkillTestMatcher
  , deriveJSON defaultOptions ''SkillTestResultMatcher
  , deriveJSON defaultOptions ''SkillTestValueMatcher
  ]

instance FromJSON SkillTestMatcher where
  parseJSON = withObject "SkillTestMatcher" $ \o -> do
    t :: Text <- o .: "tag"
    case t of
      "AnySkillTestType" -> pure AnySkillTest
      _ -> genericParseJSON defaultOptions (Object o)
