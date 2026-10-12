{-# LANGUAGE TemplateHaskell #-}

{- | What the debug panel's skill test mode asks the engine to run. See
@DebugBeginSkillTest@ in "Arkham.Message".

A real skill test, built from plain data instead of from a card's ability, so
an effect the engine does not support can still be played out with commits,
reactions, token effects and the pass/fail consequences the target already
knows how to apply.
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
  {- ^ The card the test is "on", for effects that care (\"a skill test on a
  Spell\"). Only its traits and identity are seen: the card's own text does not
  run, because nothing it implements is the test's source.
  -}
  , debugTestDifficulty :: DebugTestDifficulty
  }
  deriving stock (Show, Eq, Ord, Data)

$(deriveJSON defaultOptions ''DebugTestSkill)
$(deriveJSON defaultOptions ''DebugTestDifficulty)
$(deriveJSON (aesonOptions $ Just "debugTest") ''DebugSkillTestSpec)
