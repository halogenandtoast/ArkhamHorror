module Arkham.PlayerCard
  ( lookupPlayerCard
  , lookupPlayerCardDef
  , genPlayerCard
  , lookupPlayerCardName
  , allPlayerCards
  , basePlayerCard
  )
where

import Arkham.Prelude

import Arkham.Enemy.Cards (allPlayerEnemyCards)
import Arkham.Event.Cards (allEventCards)
import Arkham.Treachery.Cards (allPlayerTreacheryCards)
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait

genPlayerCard :: MonadRandom m => CardCode -> m PlayerCard
genPlayerCard cardCode = lookupPlayerCard cardCode <$> getRandom

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCard :: CardCode -> CardId -> PlayerCard
lookupPlayerCard cardCode cardId = MkPlayerCard
  { pcId = cardId
  , pcDef = lookupPlayerCardDef cardCode
  , pcBearer = Nothing
  }

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode CardDef
allPlayerCards = allPlayerEnemyCards <> allPlayerTreacheryCards <> allEventCards <> mapFromList
  [ ("01025", viciousBlow)
  , ("01039", deduction)
  , ("01053", opportunist)
  , ("01067", fearless)
  , ("01081", survivalInstinct)
  , ("01089", guts)
  , ("01090", perception)
  , ("01091", overpower)
  , ("01092", manualDexterity)
  , ("01093", unexpectedCourage)
  , ("02026", doubleOrNothing)
  , ("04153", trueUnderstanding)
  ]

basePlayerCard
  :: CardCode
  -> Name
  -> Int
  -> CardType
  -> ClassSymbol
  -> CardDef
basePlayerCard cardCode name cost cardType classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Just (StaticCost cost)
  , cdLevel = 0
  , cdCardType = cardType
  , cdWeakness = False
  , cdClassSymbol = Just classSymbol
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdWindows = mempty
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdUnique = False
  }

skill :: CardCode -> Name -> [SkillType] -> ClassSymbol -> CardDef
skill cardCode name skills classSymbol =
  (basePlayerCard cardCode name 0 SkillType classSymbol)
    { cdSkills = skills
    }

viciousBlow :: CardDef
viciousBlow =
  (skill "01025" "Vicious Blow" [SkillCombat] Guardian)
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
survivalInstinct =
  (skill "01081" "Survival Instrinct" [SkillAgility] Survivor)
    { cdCardTraits = setFromList [Innate]
    }

guts :: CardDef
guts =
  (skill "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

perception :: CardDef
perception =
  (skill "01090" "Perceptions" [SkillIntellect, SkillIntellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

overpower :: CardDef
overpower =
  (skill "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
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
doubleOrNothing =
  (skill "02026" "Double or Nothing" [SkillWild] Rogue)
    { cdCardTraits = singleton Fortune
    , cdCommitRestrictions = [MaxOnePerTest]
    }

trueUnderstanding :: CardDef
trueUnderstanding =
  (skill "04153" "True Understanding" [SkillWild] Seeker)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [ScenarioAbility]
    }
