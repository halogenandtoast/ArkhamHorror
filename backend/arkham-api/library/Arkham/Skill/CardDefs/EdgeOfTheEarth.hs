module Arkham.Skill.CardDefs.EdgeOfTheEarth where

import Arkham.Skill.CardDefs.Import

defensiveStance1 :: CardDef
defensiveStance1 =
  (skill "08024" "Defensive Stance" [] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }

surveyTheArea1 :: CardDef
surveyTheArea1 =
  (skill "08037" "Survey the Area" [] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }

savant1 :: CardDef
savant1 =
  (skill "08052" "Savant" [#wild] Rogue)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = Just 1
    }

occultTheory1 :: CardDef
occultTheory1 =
  (skill "08065" "Occult Theory" [] Mystic)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }

strengthInNumbers1 :: CardDef
strengthInNumbers1 =
  (skill "08077" "Strength in Numbers" [#wild] Survivor)
    { cdCardTraits = setFromList [Innate, Synergy]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }

dauntlessSpirit1 :: CardDef
dauntlessSpirit1 =
  (skill "08078" "Dauntless Spirit" [] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }
