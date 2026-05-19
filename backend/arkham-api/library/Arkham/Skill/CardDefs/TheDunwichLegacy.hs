module Arkham.Skill.CardDefs.TheDunwichLegacy where

import Arkham.Skill.CardDefs.Import

doubleOrNothing :: CardDef
doubleOrNothing =
  (skill "02026" "Double or Nothing" [#wild] Rogue)
    { cdCardTraits = singleton Fortune
    , cdCommitRestrictions = [MaxOnePerTest]
    }

deduction2 :: CardDef
deduction2 =
  (skill "02150" "Deduction" [#intellect, #intellect] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["60275"]
    }

defiance :: CardDef
defiance =
  (skill "02190" "Defiance" [#wild] Mystic)
    { cdCardTraits = singleton Innate
    , cdAlternateCardCodes = ["60418"]
    }

riseToTheOccasion :: CardDef
riseToTheOccasion =
  (skill "02192" "Rise to the Occasion" [#wild, #wild, #wild] Survivor)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 2]
    }

inquiringMind :: CardDef
inquiringMind =
  (skill "02227" "Inquiring Mind" [#wild, #wild, #wild] Seeker)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyIfYourLocationHasClues]
    }

quickThinking :: CardDef
quickThinking =
  (skill "02229" "Quick Thinking" [#wild] Rogue)
    { cdCardTraits = singleton Innate
    }

opportunist2 :: CardDef
opportunist2 =
  (skill "02231" "Opportunist" [#wild] Rogue)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [OnlyYourTest]
    , cdLevel = Just 2
    }

survivalInstinct2 :: CardDef
survivalInstinct2 =
  (skill "02235" "Survival Instinct" [#agility, #agility] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = Just 2
    }

leadership :: CardDef
leadership =
  (skill "02260" "Leadership" [#wild] Guardian)
    { cdCardTraits = singleton Practiced
    }

fearless2 :: CardDef
fearless2 =
  (skill "02268" "Fearless" [#willpower, #willpower] Mystic)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["12069"]
    }

strokeOfLuck2 :: CardDef
strokeOfLuck2 =
  (skill "02271" "Stroke of Luck" [#wild] Survivor)
    { cdCardTraits = setFromList [Innate, Fortune]
    , cdLevel = Just 2
    , cdCommitRestrictions = [OnlyYourTest]
    }

viciousBlow2 :: CardDef
viciousBlow2 =
  (skill "02299" "Vicious Blow" [#combat, #combat] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["60176"]
    }
