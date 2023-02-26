module Arkham.Skill.Cards where

import Arkham.Prelude

import Arkham.Action qualified as Action
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

skill :: CardCode -> Name -> [SkillIcon] -> ClassSymbol -> CardDef 'SkillType
skill cardCode name icons classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardSubType = Nothing
  , cdClassSymbols = singleton classSymbol
  , cdSkills = icons
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdVengeancePoints = Nothing
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
  , cdLocationSymbol = Nothing
  , cdLocationRevealedSymbol = Nothing
  , cdLocationConnections = []
  , cdLocationRevealedConnections = []
  , cdPurchaseMentalTrauma = Nothing
  , cdCanReplace = True
  }

allPlayerSkillCards :: HashMap CardCode (CardDef 'SkillType)
allPlayerSkillCards = mapFromList $ concatMap
  toCardCodePairs
  [ ableBodied
  , allIn5
  , cunning
  , curiosity
  , daring
  , deduction
  , deduction2
  , defiance
  , defiance2
  , desperateSearch
  , doubleOrNothing
  , enraptured
  , eureka
  , fearless
  , fearless2
  , guts
  , hatchetMan
  , inquiringMind
  , inspiringPresence
  , intrepid
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
  , perception2
  , prophesy
  , quickThinking
  , recklessAssault
  , resourceful
  , riseToTheOccasion
  , runForYourLife
  , sayYourPrayers
  , sealOfTheElderSign5
  , steadfast
  , strokeOfLuck2
  , stunningBlow
  , survivalInstinct
  , survivalInstinct2
  , takeHeart
  , takeTheInitiative
  , theHomeFront
  , torrentOfPower
  , trueUnderstanding
  , unexpectedCourage
  , unexpectedCourage2
  , viciousBlow
  , viciousBlow2
  , watchThis
  ]

viciousBlow :: CardDef 'SkillType
viciousBlow = (skill "01025" "Vicious Blow" [#combat] Guardian)
  { cdCardTraits = setFromList [Practiced]
  , cdAlternateCardCodes = ["01525", "60119"]
  }

deduction :: CardDef 'SkillType
deduction = (skill "01039" "Deduction" [#intellect] Seeker)
  { cdCardTraits = setFromList [Practiced]
  , cdAlternateCardCodes = ["01539", "60219"]
  }

opportunist :: CardDef 'SkillType
opportunist = (skill "01053" "Opportunist" [#wild] Rogue)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [OnlyYourTest]
  , cdAlternateCardCodes = ["01553", "60319"]
  }

fearless :: CardDef 'SkillType
fearless = (skill "01067" "Fearless" [#willpower] Mystic)
  { cdCardTraits = setFromList [Innate]
  , cdAlternateCardCodes = ["01567"]
  }

survivalInstinct :: CardDef 'SkillType
survivalInstinct = (skill "01081" "Survival Instinct" [#agility] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdAlternateCardCodes = ["01581"]
  }

guts :: CardDef 'SkillType
guts = (skill "01089" "Guts" [#willpower, #willpower] Neutral)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01589"]
  }

perception :: CardDef 'SkillType
perception =
  (skill "01090" "Perception" [#intellect, #intellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01590"]
    }

overpower :: CardDef 'SkillType
overpower = (skill "01091" "Overpower" [#combat, #combat] Neutral)
  { cdCardTraits = setFromList [Practiced]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01591"]
  }

manualDexterity :: CardDef 'SkillType
manualDexterity =
  (skill "01092" "Manual Dexterity" [#agility, #agility] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01592"]
    }

unexpectedCourage :: CardDef 'SkillType
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [#wild, #wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01593"]
    }

doubleOrNothing :: CardDef 'SkillType
doubleOrNothing = (skill "02026" "Double or Nothing" [#wild] Rogue)
  { cdCardTraits = singleton Fortune
  , cdCommitRestrictions = [MaxOnePerTest]
  }

deduction2 :: CardDef 'SkillType
deduction2 = (skill "02150" "Deduction" [#intellect, #intellect] Seeker
             )
  { cdCardTraits = setFromList [Practiced, Expert]
  , cdLevel = 2
  }

defiance :: CardDef 'SkillType
defiance = (skill "02190" "Defiance" [#wild] Mystic)
  { cdCardTraits = singleton Innate
  , cdAlternateCardCodes = ["60418"]
  }

riseToTheOccasion :: CardDef 'SkillType
riseToTheOccasion = (skill
                      "02192"
                      "Rise to the Occasion"
                      [#wild, #wild, #wild]
                      Survivor
                    )
  { cdCardTraits = singleton Innate
  , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 2]
  }

inquiringMind :: CardDef 'SkillType
inquiringMind =
  (skill "02227" "Inquiring Mind" [#wild, #wild, #wild] Seeker)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyIfYourLocationHasClues]
    }

quickThinking :: CardDef 'SkillType
quickThinking = (skill "02229" "Quick Thinking" [#wild] Rogue)
  { cdCardTraits = singleton Innate
  }

opportunist2 :: CardDef 'SkillType
opportunist2 = (skill "02231" "Opportunist" [#wild] Rogue)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdCommitRestrictions = [OnlyYourTest]
  , cdLevel = 2
  }

survivalInstinct2 :: CardDef 'SkillType
survivalInstinct2 =
  (skill "02235" "Survival Instinct" [#agility, #agility] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = 2
    }

leadership :: CardDef 'SkillType
leadership = (skill "02260" "Leadership" [#wild] Guardian)
  { cdCardTraits = singleton Practiced
  }

fearless2 :: CardDef 'SkillType
fearless2 = (skill "02268" "Fearless" [#willpower, #willpower] Mystic)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdLevel = 2
  }

strokeOfLuck2 :: CardDef 'SkillType
strokeOfLuck2 = (skill "02271" "Stroke of Luck" [#wild] Survivor)
  { cdCardTraits = setFromList [Innate, Fortune]
  , cdLevel = 2
  , cdCommitRestrictions = [OnlyYourTest]
  }

viciousBlow2 :: CardDef 'SkillType
viciousBlow2 =
  (skill "02299" "Vicious Blow" [#combat, #combat] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = 2
    }

theHomeFront :: CardDef 'SkillType
theHomeFront =
  (skill "03007" "The Home Front" (replicate 4 #combat) Neutral)
    { cdCardTraits = setFromList [Practiced, Expert]
    }

resourceful :: CardDef 'SkillType
resourceful = (skill
                "03039"
                "Resourceful"
                [#intellect, #combat, #agility]
                Survivor
              )
  { cdCardTraits = singleton Innate
  }

sayYourPrayers :: CardDef 'SkillType
sayYourPrayers = (skill
                   "03116"
                   "Say Your Prayers"
                   [ #willpower
                   , #willpower
                   , #willpower
                   , #willpower
                   ]
                   Neutral
                 )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

desperateSearch :: CardDef 'SkillType
desperateSearch = (skill
                    "03117"
                    "Desperate Search"
                    [ #intellect
                    , #intellect
                    , #intellect
                    , #intellect
                    ]
                    Neutral
                  )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

recklessAssault :: CardDef 'SkillType
recklessAssault = (skill
                    "03118"
                    "Reckless Assault"
                    [#combat, #combat, #combat, #combat]
                    Neutral
                  )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

runForYourLife :: CardDef 'SkillType
runForYourLife = (skill
                   "03119"
                   "Run For Your Life"
                   [#agility, #agility, #agility, #agility]
                   Neutral
                 )
  { cdCardTraits = singleton Desperate
  , cdCommitRestrictions =
    [ MaxOnePerTest
    , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
    ]
  }

inspiringPresence :: CardDef 'SkillType
inspiringPresence = (skill "03228" "Inspiring Presence" [#willpower, #intellect, #combat] Guardian)
  { cdCardTraits = singleton Innate
  }

eureka :: CardDef 'SkillType
eureka = (skill "03231" "Eureka!" [#willpower, #intellect, #agility] Seeker)
  { cdCardTraits = singleton Innate
  }

watchThis :: CardDef 'SkillType
watchThis = (skill "03233" "\"Watch this!\"" [#willpower, #combat, #agility] Rogue)
  { cdCardTraits = singleton Gambit
  , cdCommitRestrictions = [OnlyYourTest]
  }

torrentOfPower :: CardDef 'SkillType
torrentOfPower = (skill "03235" "Torrent of Power" [#wild] Mystic)
  { cdCardTraits = singleton Practiced
  }

notWithoutAFight :: CardDef 'SkillType
notWithoutAFight = (skill "03272" "\"Not without a fight!\"" [#willpower, #combat, #agility] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [SelfCanCommitWhen $ InvestigatorEngagedWith AnyEnemy]
  }

sealOfTheElderSign5 :: CardDef 'SkillType
sealOfTheElderSign5 = (skill "03312" "Seal of the Elder Sign" [#wild] Mystic)
  { cdCardTraits = setFromList [Spell, Expert]
  , cdLevel = 5
  }

lastChance :: CardDef 'SkillType
lastChance = (skill "04036" "Last Chance" [#wild, #wild, #wild, #wild, #wild] Survivor)
  { cdCardTraits = singleton Gambit
  , cdCommitRestrictions = [OnlyCardCommittedToTest]
  }

stunningBlow :: CardDef 'SkillType
stunningBlow = (skill "04112" "Stunning Blow" [#combat] Survivor)
  { cdCardTraits = singleton Practiced
  }

takeTheInitiative :: CardDef 'SkillType
takeTheInitiative = (skill "04150" "Take the Initiative" [#wild, #wild, #wild] Guardian)
  { cdCardTraits = setFromList [Practiced, Bold]
  , cdCommitRestrictions = [OnlyYourTest]
  }

trueUnderstanding :: CardDef 'SkillType
trueUnderstanding = (skill "04153" "True Understanding" [#wild] Seeker)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [ScenarioAbility]
  }

hatchetMan :: CardDef 'SkillType
hatchetMan = (skill "04155" "Hatchet Man" [#agility] Rogue)
  { cdCardTraits = singleton Practiced
  }

enraptured :: CardDef 'SkillType
enraptured = (skill "04157" "Enraptured" [#intellect] Mystic)
  { cdCardTraits = singleton Practiced
  }

intrepid :: CardDef 'SkillType
intrepid = (skill "04192" "Intrepid" [#willpower] Guardian)
  { cdCardTraits = singleton Innate
  }

defiance2 :: CardDef 'SkillType
defiance2 = (skill "04198" "Defiance" [#wild] Mystic)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdLevel = 2
  }

takeHeart :: CardDef 'SkillType
takeHeart = (skill "04201" "Take Heart" [] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["60519"]
  }

allIn5 :: CardDef 'SkillType
allIn5 = (skill "04309" "All In" [#wild, #wild] Rogue)
  { cdCardTraits = singleton Fortune
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdLevel = 5
  }

steadfast :: CardDef 'SkillType
steadfast = (skill "05022" "Steadfast" [#willpower, #combat] Guardian)
  { cdCardTraits = singleton Innate
  }

curiosity :: CardDef 'SkillType
curiosity = (skill "05026" "Curiosity" [#willpower, #intellect] Seeker)
  { cdCardTraits = singleton Innate
  }

cunning :: CardDef 'SkillType
cunning = (skill "05030" "Cunning" [#intellect, #agility] Rogue)
  { cdCardTraits = singleton Innate
  }

prophesy :: CardDef 'SkillType
prophesy = (skill "05034" "Prophesy" [#wild] Mystic)
  { cdCardTraits = singleton Practiced
  }

ableBodied :: CardDef 'SkillType
ableBodied = (skill "05038" "Able Bodied" [#combat, #agility] Survivor)
  { cdCardTraits = singleton Innate
  }

daring :: CardDef 'SkillType
daring = (skill "06111" "Daring" [#wild, #wild, #wild] Guardian)
  { cdCardTraits = singleton Innate
  , cdCommitRestrictions = [OnlyTestWithActions [Action.Fight, Action.Evade]]
  }

overpower2 :: CardDef 'SkillType
overpower2 = (skill "60126" "Overpower" [#combat, #combat, #combat] Guardian)
  { cdCardTraits = setFromList [Practiced, Expert]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdLevel = 2
  }

perception2 :: CardDef 'SkillType
perception2 =
  (skill "60228" "Perception" [#intellect, #intellect, #intellect] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

neitherRainNorSnow :: CardDef 'SkillType
neitherRainNorSnow = (skill
                       "60502"
                       "Neither Rain nor Snow"
                       [#wild, #wild, #wild]
                       Survivor
                     )
  { cdCardTraits = setFromList [Innate, Developed]
  }

unexpectedCourage2 :: CardDef 'SkillType
unexpectedCourage2 =
  (skill "60526" "Unexpected Courage" [#wild, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = 2
    }
