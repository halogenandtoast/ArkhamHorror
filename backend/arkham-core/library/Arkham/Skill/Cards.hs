module Arkham.Skill.Cards where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait

skill :: CardCode -> Name -> [SkillType] -> ClassSymbol -> CardDef
skill cardCode name skills classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = SkillType
  , cdWeakness = False
  , cdClassSymbol = Just classSymbol
  , cdSkills = skills
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdWindows = mempty
  , cdAction = Nothing
  , cdRevelation = True
  , cdVictoryPoints = Nothing
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  }

allPlayerSkillCards :: HashMap CardCode CardDef
allPlayerSkillCards = mapFromList $ map
  (toCardCode &&& id)
  [ deduction
  , doubleOrNothing
  , fearless
  , guts
  , manualDexterity
  , opportunist
  , overpower
  , perception
  , survivalInstinct
  , trueUnderstanding
  , unexpectedCourage
  , viciousBlow
  ]
viciousBlow :: CardDef
viciousBlow = (skill "01025" "Vicious Blow" [SkillCombat] Guardian)
  { cdCardTraits = setFromList [Practiced]
  }

deduction :: CardDef
deduction = (skill "01039" "Deduction" [SkillIntellect] Seeker)
  { cdCardTraits = setFromList [Practiced]
  }

opportunist :: CardDef
opportunist = (skill "01053" "Opportunist" [SkillWild] Rogue)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [OnlyYourTest]
  }

fearless :: CardDef
fearless = (skill "01067" "Fearless" [SkillWillpower] Mystic)
  { cdCardTraits = setFromList [Innate]
  }

survivalInstinct :: CardDef
survivalInstinct = (skill "01081" "Survival Instinct" [SkillAgility] Survivor)
  { cdCardTraits = setFromList [Innate]
  }

guts :: CardDef
guts = (skill "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  }

perception :: CardDef
perception =
  (skill "01090" "Perception" [SkillIntellect, SkillIntellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

overpower :: CardDef
overpower = (skill "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
  { cdCardTraits = setFromList [Practiced]
  , cdCommitRestrictions = [MaxOnePerTest]
  }

manualDexterity :: CardDef
manualDexterity =
  (skill "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

unexpectedCourage :: CardDef
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

doubleOrNothing :: CardDef
doubleOrNothing = (skill "02026" "Double or Nothing" [SkillWild] Rogue)
  { cdCardTraits = singleton Fortune
  , cdCommitRestrictions = [MaxOnePerTest]
  }

trueUnderstanding :: CardDef
trueUnderstanding = (skill "04153" "True Understanding" [SkillWild] Seeker)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [ScenarioAbility]
  }
