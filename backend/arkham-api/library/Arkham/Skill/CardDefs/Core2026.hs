module Arkham.Skill.CardDefs.Core2026 where

import Arkham.Skill.CardDefs.Import

outOfSight :: CardDef
outOfSight =
  (skill "12053" "Out of Sight" [#agility] Rogue)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

outOfSight3 :: CardDef
outOfSight3 =
  (skill "12057" "Out of Sight" [#agility, #agility, #agility] Rogue)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 3
    }

soulLink :: CardDef
soulLink =
  (skill "12067" "Soul Link" [#wild, #wild, #wild] Mystic)
    { cdCardTraits = setFromList [Innate, Spell]
    }

slippery :: CardDef
slippery =
  (skill "12080" "Slippery" [#agility] Survivor)
    { cdCardTraits = setFromList [Practiced]
    }

timelyIntervention :: CardDef
timelyIntervention =
  (skill "12081" "Timely Intervention" [#willpower, #agility, #wild] Survivor)
    { cdCardTraits = singleton Fortune
    , cdCommitRestrictions = [MaxOnePerTest, CanCommitAfterRevealingTokens]
    }

onTheBrink2 :: CardDef
onTheBrink2 =
  (skill "12084" "On the Brink" [#wild, #wild] Survivor)
    { cdCardTraits = setFromList [Gambit, Desperate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }
