{-# LANGUAGE TemplateHaskell #-}

{- | What @DebugBeginSkillTest@ asks the engine to run: a real skill test built
from plain data instead of from a card's ability.
-}
module Arkham.Debug.SkillTest where

import Arkham.Action (Action)
import Arkham.Card.Id (CardId)
import Arkham.Id
import Arkham.Json
import Arkham.Prelude
import Arkham.SkillType (SkillType)
import Arkham.Target (Target)
import Data.Aeson.TH

-- | What the test is "of".
data DebugTestSkill
  = DebugTestOneSkill SkillType
  | -- | Two or more skills added together ("test willpower + intellect").
    DebugTestCombined [SkillType]
  | -- | No skill, just a base value ("test (3)"). Only wild icons count.
    DebugTestFixed Int
  deriving stock (Show, Eq, Ord, Data)

data DebugTestDifficulty
  = DebugDifficultyFixed Int
  | -- | The target's own value: an enemy's fight or evade, a location's shroud.
    DebugDifficultyOfTarget
  deriving stock (Show, Eq, Ord, Data)

data DebugSkillTestSpec = DebugSkillTestSpec
  { debugTestInvestigator :: InvestigatorId
  , debugTestSkill :: DebugTestSkill
  , debugTestAction :: Maybe Action
  -- ^ Fight, Evade, Investigate or Parley; 'Nothing' for a plain test.
  , debugTestTarget :: Maybe Target
  -- ^ An enemy for fight/evade, a location for investigate; optional otherwise.
  , debugTestSourceCard :: Maybe CardId
  {- ^ The card the test is "on", for effects that care ("a test on a Spell").
  Only its traits and identity are seen; the card's own text does not run.
  -}
  , debugTestDifficulty :: DebugTestDifficulty
  }
  deriving stock (Show, Eq, Ord, Data)

mconcat
  [ deriveJSON defaultOptions ''DebugTestSkill
  , deriveJSON defaultOptions ''DebugTestDifficulty
  , deriveJSON (aesonOptions $ Just "debugTest") ''DebugSkillTestSpec
  ]
