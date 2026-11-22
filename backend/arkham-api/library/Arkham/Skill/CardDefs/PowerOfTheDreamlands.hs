module Arkham.Skill.CardDefs.PowerOfTheDreamlands where

import Arkham.Skill.CardDefs.Import

dreamMastery :: CardDef
dreamMastery =
  (skill "14008" "Dream Mastery" [#willpower, #combat] Guardian)
    { cdCardTraits = setFromList [Practiced]
    }
