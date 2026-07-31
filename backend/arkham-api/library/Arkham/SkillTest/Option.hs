{-# LANGUAGE TemplateHaskell #-}

module Arkham.SkillTest.Option where

import {-# SOURCE #-} Arkham.Criteria
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Question (UI)
import Data.Aeson.TH

{- | The category of a 'SkillTestOption'. Determines how the option interleaves
with the original skill-test option provided by the game engine.
-}
data SkillTestOptionKind
  = -- | the original option
    OriginalOptionKind
  | -- | an additional option added by an effect
    AdditionalOptionKind
  | {- | an option that blocks the original option
    (e.g. Mariner's Compass should happen before original)
    -}
    BlockingOptionKind
  | -- | an option that can only be chosen if the OriginalOptionKind is still available
    PreOriginalOptionKind
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

{- | Mark an option as the consequence the skill test was initiated for, so it
takes its place in the ST.7 ordering choice instead of pre-empting the other
results.
-}
originalOption :: SkillTestOption -> SkillTestOption
originalOption sto = sto {kind = OriginalOptionKind}

$(deriveJSON defaultOptions ''SkillTestOptionKind)
$(deriveJSON defaultOptions ''SkillTestOption)
