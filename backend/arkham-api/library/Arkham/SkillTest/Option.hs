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
  , whenUnmet :: Maybe [Message]
  {- ^ Resolved in place of the option when 'criteria' are not met at the point
  the option is discarded for good.

  A gated option is otherwise dropped in silence, which hides /why/ nothing
  happened. The House Always Wins gated its Cultist "gain 3 resources" on being
  able to gain resources, so O'Bannion's Thug in the player's threat area made
  the option vanish from the prompt with no explanation, and correct behaviour
  read as a bug (#6017). An effect whose own handler reports its refusal can
  pass its body here: the dead option stays off the prompt -- nobody is asked to
  click a choice that cannot do anything -- and the refusal still reaches the
  log. 'Nothing' keeps the silent drop.

  Judged where the option dies, not at the end of the test. The criteria are
  re-checked every time the remaining options are offered, so an option blocked
  when the prompt was built still resolves normally once an earlier option clears
  the blocker -- which is how killing the Thug with the same test's fight pays
  those resources out after all.
  -}
  }
  deriving stock (Show, Ord, Eq, Generic, Data)

setOptionCriteria :: Criterion -> SkillTestOption -> SkillTestOption
setOptionCriteria c sto = sto {criteria = Just c}

optionWhenExists :: Exists a => a -> SkillTestOption -> SkillTestOption
optionWhenExists a = setOptionCriteria (exists a)

-- | Resolve @msgs@ instead of the option when its criteria are never met. See 'whenUnmet'.
optionWhenUnmet :: [Message] -> SkillTestOption -> SkillTestOption
optionWhenUnmet msgs sto = sto {whenUnmet = Just msgs}

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
