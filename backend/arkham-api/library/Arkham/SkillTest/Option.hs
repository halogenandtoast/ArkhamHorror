{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Option where

import {-# SOURCE #-} Arkham.Criteria
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Question (UI)
import Data.Aeson.TH

-- | The category of a 'SkillTestOption'. Determines how the option interleaves
-- with the original skill-test option provided by the game engine.
data SkillTestOptionKind
  = OriginalOptionKind
  -- ^ the original option
  | AdditionalOptionKind
  -- ^ an additional option added by an effect
  | BlockingOptionKind
  -- ^ an option that blocks the original option
  -- (e.g. Mariner's Compass should happen before original)
  | PreOriginalOptionKind
  -- ^ an option that can only be chosen if the OriginalOptionKind is still available
  deriving stock (Show, Ord, Eq, Generic, Data)

-- | A choice presented to the player as part of skill-test resolution.
data SkillTestOption = SkillTestOption
  { option :: UI Message
  , kind :: SkillTestOptionKind
  , criteria :: Maybe Criterion
  }
  deriving stock (Show, Ord, Eq, Generic, Data)

setOptionCriteria :: Criterion -> SkillTestOption -> SkillTestOption
setOptionCriteria c sto = sto {criteria = Just c}

optionWhenExists :: Exists a => a -> SkillTestOption -> SkillTestOption
optionWhenExists a = setOptionCriteria (exists a)

preOriginalOption :: SkillTestOption -> SkillTestOption
preOriginalOption sto = sto {kind = PreOriginalOptionKind}

$(deriveJSON defaultOptions ''SkillTestOptionKind)
$(deriveJSON defaultOptions ''SkillTestOption)
