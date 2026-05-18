module Arkham.Skill.CardDefs.TheForgottenAge where

import Arkham.Skill.CardDefs.Import

lastChance :: CardDef
lastChance =
  (skill "04036" "Last Chance" [#wild, #wild, #wild, #wild, #wild] Survivor)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyCardCommittedToTest]
    , cdOutOfPlayEffects = [InHandEffect]
    }

stunningBlow :: CardDef
stunningBlow =
  (skill "04112" "Stunning Blow" [#combat] Survivor)
    { cdCardTraits = singleton Practiced
    }

takeTheInitiative :: CardDef
takeTheInitiative =
  (skill "04150" "Take the Initiative" [#wild, #wild, #wild] Guardian)
    { cdCardTraits = setFromList [Practiced, Bold]
    , cdCommitRestrictions = [OnlyYourTest]
    , cdOutOfPlayEffects = [InHandEffect]
    }

trueUnderstanding :: CardDef
trueUnderstanding =
  (skill "04153" "True Understanding" [#wild] Seeker)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [ScenarioAbility]
    }

hatchetMan :: CardDef
hatchetMan =
  (skill "04155" "Hatchet Man" [#agility] Rogue)
    { cdCardTraits = singleton Practiced
    }

enraptured :: CardDef
enraptured =
  (skill "04157" "Enraptured" [#intellect] Mystic)
    { cdCardTraits = singleton Practiced
    }

intrepid :: CardDef
intrepid =
  (skill "04192" "Intrepid" [#willpower] Guardian)
    { cdCardTraits = singleton Innate
    }

defiance2 :: CardDef
defiance2 =
  (skill "04198" "Defiance" [#wild] Mystic)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = Just 2
    , cdCommitTrigger = True
    }

takeHeart :: CardDef
takeHeart =
  (skill "04201" "Take Heart" [] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["60519"]
    }

allIn5 :: CardDef
allIn5 =
  (skill "04309" "All In" [#wild, #wild] Rogue)
    { cdCardTraits = singleton Fortune
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 5
    , cdCommitTrigger = True
    }
