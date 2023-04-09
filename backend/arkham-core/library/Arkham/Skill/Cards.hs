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

skill :: CardCode -> Name -> [SkillIcon] -> ClassSymbol -> CardDef
skill cardCode name icons classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardType = SkillType
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

allPlayerSkillCards :: HashMap CardCode CardDef
allPlayerSkillCards = mapFromList $ concatMap
  toCardCodePairs
  [ ableBodied
  , allIn5
  , anythingYouCanDoBetter
  , arrogance
  , copycat3
  , cunning
  , curiosity
  , daredevil
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
  , manualDexterity2
  , neitherRainNorSnow
  , nimble
  , notWithoutAFight
  , opportunist
  , opportunist2
  , overpower
  , overpower2
  , perception
  , perception2
  , prescient
  , prophesy
  , quickThinking
  , reckless
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

viciousBlow :: CardDef
viciousBlow = (skill "01025" "Vicious Blow" [#combat] Guardian)
  { cdCardTraits = setFromList [Practiced]
  , cdAlternateCardCodes = ["01525", "60119"]
  }

deduction :: CardDef
deduction = (skill "01039" "Deduction" [#intellect] Seeker)
  { cdCardTraits = setFromList [Practiced]
  , cdAlternateCardCodes = ["01539", "60219"]
  }

opportunist :: CardDef
opportunist = (skill "01053" "Opportunist" [#wild] Rogue)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [OnlyYourTest]
  , cdAlternateCardCodes = ["01553", "60319"]
  }

fearless :: CardDef
fearless = (skill "01067" "Fearless" [#willpower] Mystic)
  { cdCardTraits = setFromList [Innate]
  , cdAlternateCardCodes = ["01567"]
  }

survivalInstinct :: CardDef
survivalInstinct = (skill "01081" "Survival Instinct" [#agility] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdAlternateCardCodes = ["01581"]
  }

guts :: CardDef
guts = (skill "01089" "Guts" [#willpower, #willpower] Neutral)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01589"]
  }

perception :: CardDef
perception =
  (skill "01090" "Perception" [#intellect, #intellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01590"]
    }

overpower :: CardDef
overpower = (skill "01091" "Overpower" [#combat, #combat] Neutral)
  { cdCardTraits = setFromList [Practiced]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["01591"]
  }

manualDexterity :: CardDef
manualDexterity =
  (skill "01092" "Manual Dexterity" [#agility, #agility] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01592"]
    }

unexpectedCourage :: CardDef
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [#wild, #wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdAlternateCardCodes = ["01593"]
    }

doubleOrNothing :: CardDef
doubleOrNothing = (skill "02026" "Double or Nothing" [#wild] Rogue)
  { cdCardTraits = singleton Fortune
  , cdCommitRestrictions = [MaxOnePerTest]
  }

deduction2 :: CardDef
deduction2 = (skill "02150" "Deduction" [#intellect, #intellect] Seeker
             )
  { cdCardTraits = setFromList [Practiced, Expert]
  , cdLevel = 2
  }

defiance :: CardDef
defiance = (skill "02190" "Defiance" [#wild] Mystic)
  { cdCardTraits = singleton Innate
  , cdAlternateCardCodes = ["60418"]
  }

riseToTheOccasion :: CardDef
riseToTheOccasion = (skill
                      "02192"
                      "Rise to the Occasion"
                      [#wild, #wild, #wild]
                      Survivor
                    )
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
quickThinking = (skill "02229" "Quick Thinking" [#wild] Rogue)
  { cdCardTraits = singleton Innate
  }

opportunist2 :: CardDef
opportunist2 = (skill "02231" "Opportunist" [#wild] Rogue)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdCommitRestrictions = [OnlyYourTest]
  , cdLevel = 2
  }

survivalInstinct2 :: CardDef
survivalInstinct2 =
  (skill "02235" "Survival Instinct" [#agility, #agility] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdLevel = 2
    }

leadership :: CardDef
leadership = (skill "02260" "Leadership" [#wild] Guardian)
  { cdCardTraits = singleton Practiced
  }

fearless2 :: CardDef
fearless2 = (skill "02268" "Fearless" [#willpower, #willpower] Mystic)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdLevel = 2
  }

strokeOfLuck2 :: CardDef
strokeOfLuck2 = (skill "02271" "Stroke of Luck" [#wild] Survivor)
  { cdCardTraits = setFromList [Innate, Fortune]
  , cdLevel = 2
  , cdCommitRestrictions = [OnlyYourTest]
  }

viciousBlow2 :: CardDef
viciousBlow2 =
  (skill "02299" "Vicious Blow" [#combat, #combat] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = 2
    }

theHomeFront :: CardDef
theHomeFront =
  (skill "03007" "The Home Front" (replicate 4 #combat) Neutral)
    { cdCardTraits = setFromList [Practiced, Expert]
    }

resourceful :: CardDef
resourceful = (skill
                "03039"
                "Resourceful"
                [#intellect, #combat, #agility]
                Survivor
              )
  { cdCardTraits = singleton Innate
  }

sayYourPrayers :: CardDef
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

desperateSearch :: CardDef
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

recklessAssault :: CardDef
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

runForYourLife :: CardDef
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

inspiringPresence :: CardDef
inspiringPresence = (skill "03228" "Inspiring Presence" [#willpower, #intellect, #combat] Guardian)
  { cdCardTraits = singleton Innate
  }

eureka :: CardDef
eureka = (skill "03231" "Eureka!" [#willpower, #intellect, #agility] Seeker)
  { cdCardTraits = singleton Innate
  }

watchThis :: CardDef
watchThis = (skill "03233" "\"Watch this!\"" [#willpower, #combat, #agility] Rogue)
  { cdCardTraits = singleton Gambit
  , cdCommitRestrictions = [OnlyYourTest]
  }

torrentOfPower :: CardDef
torrentOfPower = (skill "03235" "Torrent of Power" [#wild] Mystic)
  { cdCardTraits = singleton Practiced
  }

notWithoutAFight :: CardDef
notWithoutAFight = (skill "03272" "\"Not without a fight!\"" [#willpower, #combat, #agility] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [SelfCanCommitWhen $ InvestigatorEngagedWith AnyEnemy]
  }

sealOfTheElderSign5 :: CardDef
sealOfTheElderSign5 = (skill "03312" "Seal of the Elder Sign" [#wild] Mystic)
  { cdCardTraits = setFromList [Spell, Expert]
  , cdLevel = 5
  }

lastChance :: CardDef
lastChance = (skill "04036" "Last Chance" [#wild, #wild, #wild, #wild, #wild] Survivor)
  { cdCardTraits = singleton Gambit
  , cdCommitRestrictions = [OnlyCardCommittedToTest]
  }

stunningBlow :: CardDef
stunningBlow = (skill "04112" "Stunning Blow" [#combat] Survivor)
  { cdCardTraits = singleton Practiced
  }

takeTheInitiative :: CardDef
takeTheInitiative = (skill "04150" "Take the Initiative" [#wild, #wild, #wild] Guardian)
  { cdCardTraits = setFromList [Practiced, Bold]
  , cdCommitRestrictions = [OnlyYourTest]
  }

trueUnderstanding :: CardDef
trueUnderstanding = (skill "04153" "True Understanding" [#wild] Seeker)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [ScenarioAbility]
  }

hatchetMan :: CardDef
hatchetMan = (skill "04155" "Hatchet Man" [#agility] Rogue)
  { cdCardTraits = singleton Practiced
  }

enraptured :: CardDef
enraptured = (skill "04157" "Enraptured" [#intellect] Mystic)
  { cdCardTraits = singleton Practiced
  }

intrepid :: CardDef
intrepid = (skill "04192" "Intrepid" [#willpower] Guardian)
  { cdCardTraits = singleton Innate
  }

defiance2 :: CardDef
defiance2 = (skill "04198" "Defiance" [#wild] Mystic)
  { cdCardTraits = setFromList [Innate, Developed]
  , cdLevel = 2
  }

takeHeart :: CardDef
takeHeart = (skill "04201" "Take Heart" [] Survivor)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdAlternateCardCodes = ["60519"]
  }

allIn5 :: CardDef
allIn5 = (skill "04309" "All In" [#wild, #wild] Rogue)
  { cdCardTraits = singleton Fortune
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdLevel = 5
  }

steadfast :: CardDef
steadfast = (skill "05022" "Steadfast" [#willpower, #combat] Guardian)
  { cdCardTraits = singleton Innate
  }

curiosity :: CardDef
curiosity = (skill "05026" "Curiosity" [#willpower, #intellect] Seeker)
  { cdCardTraits = singleton Innate
  }

cunning :: CardDef
cunning = (skill "05030" "Cunning" [#intellect, #agility] Rogue)
  { cdCardTraits = singleton Innate
  }

prophesy :: CardDef
prophesy = (skill "05034" "Prophesy" [#wild] Mystic)
  { cdCardTraits = singleton Practiced
  }

ableBodied :: CardDef
ableBodied = (skill "05038" "Able Bodied" [#combat, #agility] Survivor)
  { cdCardTraits = singleton Innate
  }

daring :: CardDef
daring = (skill "06111" "Daring" [#wild, #wild, #wild] Guardian)
  { cdCardTraits = singleton Innate
  , cdCommitRestrictions = [OnlyTestWithActions [Action.Fight, Action.Evade]]
  }

overpower2 :: CardDef
overpower2 = (skill "60126" "Overpower" [#combat, #combat, #combat] Guardian)
  { cdCardTraits = setFromList [Practiced, Expert]
  , cdCommitRestrictions = [MaxOnePerTest]
  , cdLevel = 2
  }

perception2 :: CardDef
perception2 =
  (skill "60228" "Perception" [#intellect, #intellect, #intellect] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = 2
    }

anythingYouCanDoBetter :: CardDef
anythingYouCanDoBetter = (skill
                       "60302"
                       "Anything You Can Do, Better"
                       [#wild, #wild, #wild, #wild, #wild, #wild]
                       Rogue
                     )
  { cdCardTraits = setFromList [Innate, Developed]
  , cdCommitRestrictions = [OnlyYourTest]
  }

arrogance :: CardDef
arrogance = (skill "60303" "Arrogance" [#wildMinus] Neutral)
  { cdCardTraits = singleton Flaw
  , cdCardSubType = Just Weakness
  , cdCommitRestrictions = [MustBeCommittedToYourTest]
  }

reckless :: CardDef
reckless = (skill "60304" "Reckless" [] Neutral)
  { cdCardTraits = singleton Flaw
  , cdCardSubType = Just Weakness
  , cdCommitRestrictions = [OnlyCardCommittedToTest]
  , cdCardInHandEffects = True
  }

nimble :: CardDef
nimble = (skill "60317" "Nimble" [#agility] Rogue)
  { cdCardTraits = singleton Innate
  }

daredevil :: CardDef
daredevil = (skill "60318" "Daredevil" [#wild] Rogue)
  { cdCardTraits = setFromList [Fortune, Practiced]
  }

manualDexterity2 :: CardDef
manualDexterity2 =
  (skill "60325" "Manual Dexterity" [#agility, #agility, #agility] Rogue)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

copycat3 :: CardDef
copycat3 =
  (skill "60330" "Copycat" [#wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdLevel = 3
    }

prescient :: CardDef
prescient =
  (skill "60419" "Prescient" [#willpower] Mystic)
    { cdCardTraits = setFromList [Practiced, Augury]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

neitherRainNorSnow :: CardDef
neitherRainNorSnow = (skill "60502" "Neither Rain nor Snow" [#wild, #wild, #wild] Survivor)
  { cdCardTraits = setFromList [Innate, Developed]
  }

unexpectedCourage2 :: CardDef
unexpectedCourage2 =
  (skill "60526" "Unexpected Courage" [#wild, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = 2
    }
