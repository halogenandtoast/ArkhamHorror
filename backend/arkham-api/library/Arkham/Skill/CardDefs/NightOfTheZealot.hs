module Arkham.Skill.CardDefs.NightOfTheZealot where

import Arkham.Skill.CardDefs.Import

viciousBlow :: CardDef
viciousBlow =
  (skill "01025" "Vicious Blow" [#combat] Guardian)
    { cdCardTraits = setFromList [Practiced]
    , cdAlternateCardCodes = ["01525", "60119", "12025", "60169"]
    }

deduction :: CardDef
deduction =
  (skill "01039" "Deduction" [#intellect] Seeker)
    { cdCardTraits = setFromList [Practiced]
    , cdAlternateCardCodes = ["01539", "60219", "12039", "60267"]
    }

opportunist :: CardDef
opportunist =
  (skill "01053" "Opportunist" [#wild] Rogue)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [OnlyYourTest]
    , cdAlternateCardCodes = ["01553", "60319"]
    }

fearless :: CardDef
fearless =
  (skill "01067" "Fearless" [#willpower] Mystic)
    { cdCardTraits = setFromList [Innate]
    , cdAlternateCardCodes = ["01567"]
    }

survivalInstinct :: CardDef
survivalInstinct =
  (skill "01081" "Survival Instinct" [#agility] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdAlternateCardCodes = ["01581"]
    }

guts :: CardDef
guts =
  (skill "01089" "Guts" [#willpower, #willpower] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01589", "12090"]
    }

perception :: CardDef
perception =
  (skill "01090" "Perception" [#intellect, #intellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01590", "12093"]
    }

overpower :: CardDef
overpower =
  (skill "01091" "Overpower" [#combat, #combat] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01591", "12092"]
    }

manualDexterity :: CardDef
manualDexterity =
  (skill "01092" "Manual Dexterity" [#agility, #agility] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01592", "12091"]
    }

unexpectedCourage :: CardDef
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [#wild, #wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01593", "12094"]
    }
