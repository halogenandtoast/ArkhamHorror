module Arkham.Skill.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Name
import Arkham.SkillType
import Arkham.Trait

skill :: CardCode -> Name -> [SkillType] -> ClassSymbol -> CardDef
skill cardCode name skills classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = SkillType
  , cdCardSubType = Nothing
  , cdClassSymbols = singleton classSymbol
  , cdSkills = skills
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdCriteria = mempty
  , cdOverrideActionPlayableIfCriteriaMet = False
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = NoUses
  , cdPlayableFromDiscard = False
  , cdStage = Nothing
  , cdSlots = []
  , cdCardInHandEffects = False
  , cdCardInDiscardEffects = False
  , cdCardInSearchEffects = False
  , cdAlternateCardCodes = []
  , cdArt = unCardCode cardCode
  }

allPlayerSkillCards :: HashMap CardCode CardDef
allPlayerSkillCards = mapFromList $ concatMap
  toCardCodePairs
  [ deduction
  , deduction2
  , defiance
  , desperateSearch
  , doubleOrNothing
  , eureka
  , fearless
  , fearless2
  , guts
  , inquiringMind
  , inspiringPresence
  , lastChance
  , leadership
  , manualDexterity
  , neitherRainNorSnow
  , notWithoutAFight
  , opportunist
  , opportunist2
  , overpower
  , overpower2
  , perception
  , quickThinking
  , recklessAssault
  , resourceful
  , riseToTheOccasion
  , runForYourLife
  , sayYourPrayers
  , sealOfTheElderSign5
  , strokeOfLuck2
  , survivalInstinct
  , survivalInstinct2
  , takeHeart
  , theHomeFront
  , torrentOfPower
  , trueUnderstanding
  , unexpectedCourage
  , unexpectedCourage2
  , viciousBlow
  , viciousBlow2
  , watchThis
  ]

viciousBlow :: CardDef
viciousBlow = (skill "01025" "Vicious Blow" [SkillCombat] Guardian)
  { cdCardTraits = setFromList [Practiced]
  , cdAlternateCardCodes = ["01525", "60119"]
  }

deduction :: CardDef
deduction = (skill "01039" "Deduction" [SkillIntellect] Seeker)
  { cdCardTraits = setFromList [Practiced]
  , cdAlternateCardCodes = ["01539"]
  }

opportunist :: CardDef
opportunist = (skill "01053" "Opportunist" [SkillWild] Rogue)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [OnlyYourTest]
  , cdAlternateCardCodes = ["01553"]
  }

fearless :: CardDef
fearless = (skill "01067" "Fearless" [SkillWillpower] Mystic)
  { cdCardTraits = setFromList [Innate]
  , cdAlternateCardCodes = ["01567"]
  }

survivalInstinct :: CardDef
survivalInstinct = (skill "01081" "Survival Instinct" [SkillAgility] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdAlternateCardCodes = ["01581"]
  }

guts :: CardDef
guts = (skill "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01589"]
  }

perception :: CardDef
perception =
  (skill "01090" "Perception" [SkillIntellect, SkillIntellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01590"]
    }

overpower :: CardDef
overpower = (skill "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
  { cdCardTraits = setFromList [Practiced]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01591"]
  }

manualDexterity :: CardDef
manualDexterity =
  (skill "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01592"]
    }

unexpectedCourage :: CardDef
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01593"]
    }

doubleOrNothing :: CardDef
doubleOrNothing = (skill "02026" "Double or Nothing" [SkillWild] Rogue)
  { cdCardTraits = singleton Fortune
  , cdCommitRestrictions = [MaxOnePerTest]
  }

deduction2 :: CardDef
deduction2 = (skill "02150" "Deduction" [SkillIntellect, SkillIntellect] Seeker
             )
  { cdCardTraits = setFromList [Practiced, Expert]
  , cdLevel = 2
  }

defiance :: CardDef
defiance = (skill "02190" "Defiance" [SkillWild] Mystic)
  { cdCardTraits = singleton Innate
  }

riseToTheOccasion :: CardDef
riseToTheOccasion = (skill
                      "02192"
                      "Rise to the Occasion"
                      [SkillWild, SkillWild, SkillWild]
                      Survivor
                    )
  { cdCardTraits = singleton Innate
  , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 2]
  }

inquiringMind :: CardDef
inquiringMind =
  (skill "02227" "Inquiring Mind" [SkillWild, SkillWild, SkillWild] Seeker)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyIfYourLocationHasClues]
    }

quickThinking :: CardDef
quickThinking = (skill "02229" "Quick Thinking" [SkillWild] Rogue)
  { cdCardTraits = singleton Innate
  }

opportunist2 :: CardDef
opportunist2 = (skill "02231" "Opportunist" [SkillWild] Rogue)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdCommitRestrictions = [OnlyYourTest]
  , cdLevel = 2
  }

survivalInstinct2 :: CardDef
survivalInstinct2 =
  (skill "02235" "Survival Instinct" [SkillAgility, SkillAgility] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = 2
    }

leadership :: CardDef
leadership = (skill "02260" "Leadership" [SkillWild] Guardian)
  { cdCardTraits = singleton Practiced
  }

fearless2 :: CardDef
fearless2 = (skill "02268" "Fearless" [SkillWillpower, SkillWillpower] Mystic)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdLevel = 2
  }

strokeOfLuck2 :: CardDef
strokeOfLuck2 = (skill "02271" "Stroke of Luck" [SkillWild] Survivor)
  { cdCardTraits = setFromList [Innate, Fortune]
  , cdLevel = 2
  , cdCommitRestrictions = [OnlyYourTest]
  }

viciousBlow2 :: CardDef
viciousBlow2 =
  (skill "02299" "Vicious Blow" [SkillCombat, SkillCombat] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = 2
    }

theHomeFront :: CardDef
theHomeFront =
  (skill "03007" "The Home Front" (replicate 4 SkillCombat) Neutral)
    { cdCardTraits = setFromList [Practiced, Expert]
    }

resourceful :: CardDef
resourceful = (skill
                "03039"
                "Resourceful"
                [SkillIntellect, SkillCombat, SkillAgility]
                Survivor
              )
  { cdCardTraits = singleton Innate
  }

sayYourPrayers :: CardDef
sayYourPrayers = (skill
                   "03116"
                   "Say Your Prayers"
                   [ SkillWillpower
                   , SkillWillpower
                   , SkillWillpower
                   , SkillWillpower
                   ]
                   Neutral
                 )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

desperateSearch :: CardDef
desperateSearch = (skill
                    "03117"
                    "Desperate Search"
                    [ SkillIntellect
                    , SkillIntellect
                    , SkillIntellect
                    , SkillIntellect
                    ]
                    Neutral
                  )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

recklessAssault :: CardDef
recklessAssault = (skill
                    "03118"
                    "Reckless Assault"
                    [SkillCombat, SkillCombat, SkillCombat, SkillCombat]
                    Neutral
                  )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

runForYourLife :: CardDef
runForYourLife = (skill
                   "03119"
                   "Run For Your Life"
                   [SkillAgility, SkillAgility, SkillAgility, SkillAgility]
                   Neutral
                 )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

inspiringPresence :: CardDef
inspiringPresence = (skill "03228" "Inspiring Presence" [SkillWillpower, SkillIntellect, SkillCombat] Guardian)
  { cdCardTraits = singleton Innate
  }

eureka :: CardDef
eureka = (skill "03231" "Eureka!" [SkillWillpower, SkillIntellect, SkillAgility] Seeker)
  { cdCardTraits = singleton Innate
  }

watchThis :: CardDef
watchThis = (skill "03233" "\"Watch this!\"" [SkillWillpower, SkillCombat, SkillAgility] Rogue)
  { cdCardTraits = singleton Gambit
  , cdCommitRestrictions = [OnlyYourTest]
  }

torrentOfPower :: CardDef
torrentOfPower = (skill "03235" "Torrent of Power" [SkillWild] Mystic)
  { cdCardTraits = singleton Practiced
  }

notWithoutAFight :: CardDef
notWithoutAFight = (skill "03272" "\"Not without a fight!\"" [SkillWillpower, SkillCombat, SkillAgility] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [SelfCanCommitWhen $ InvestigatorEngagedWith AnyEnemy]
  }

sealOfTheElderSign5 :: CardDef
sealOfTheElderSign5 = (skill "03312" "Seal of the Elder Sign" [SkillWild] Mystic)
  { cdCardTraits = setFromList [Spell, Expert]
  , cdLevel = 5
  }

lastChance :: CardDef
lastChance = (skill "04036" "Last Chance" [SkillWild, SkillWild, SkillWild, SkillWild, SkillWild] Survivor)
  { cdCardTraits = singleton Gambit
  , cdCommitRestrictions = [OnlyCardCommittedToTest]
  }

trueUnderstanding :: CardDef
trueUnderstanding = (skill "04153" "True Understanding" [SkillWild] Seeker)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [ScenarioAbility]
  }

takeHeart :: CardDef
takeHeart = (skill "04201" "Take Heart" [] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["60519"]
  }

overpower2 :: CardDef
overpower2 = (skill "60126" "Overpower" [SkillCombat, SkillCombat, SkillCombat] Guardian)
  { cdCardTraits = setFromList [Practiced, Expert]
  , cdCommitRestrictions = [MaxOnePerTest]
  }

neitherRainNorSnow :: CardDef
neitherRainNorSnow = (skill
                       "60502"
                       "Neither Rain nor Snow"
                       [SkillWild, SkillWild, SkillWild]
                       Survivor
                     )
  { cdCardTraits = setFromList [Innate, Developed]
  }

unexpectedCourage2 :: CardDef
unexpectedCourage2 =
  (skill "60526" "Unexpected Courage" [SkillWild, SkillWild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = 2
    }
