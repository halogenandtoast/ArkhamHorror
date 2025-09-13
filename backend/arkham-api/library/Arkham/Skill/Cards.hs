module Arkham.Skill.Cards where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import Arkham.Customization
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Name
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Trait

skill :: CardCode -> Name -> [SkillIcon] -> ClassSymbol -> CardDef
skill cardCode name icons classSymbol =
  (emptyCardDef cardCode name SkillType)
    { cdClassSymbols = singleton classSymbol
    , cdSkills = icons
    , cdCanCommitWhenNoIcons = null icons
    }

signature :: InvestigatorId -> CardDef -> CardDef
signature iid cd = cd {cdDeckRestrictions = [Signature iid], cdLevel = Nothing}

allPlayerSkillCards :: Map CardCode CardDef
allPlayerSkillCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ ableBodied
      , accursed
      , allIn5
      , analysis
      , anythingYouCanDoBetter
      , arrogance
      , asYouWish
      , beloved
      , bruteForce1
      , calculatedRisk
      , confidence
      , contemplative
      , copycat3
      , crackShot
      , cunning
      , curiosity
      , daredevil
      , daredevil2
      , daring
      , dauntlessSpirit1
      , deduction
      , deduction2
      , defensiveStance1
      , defiance
      , defiance2
      , desperateSearch
      , determined
      , diabolicalLuck
      , doubleDown2
      , doubleOrNothing
      , dreamParasite
      , dreamsOfTheClay1
      , dreamsOfTheDeepTheDeepGate
      , enraptured
      , enraptured2
      , essenceOfTheDream
      , esotericMethod1
      , eureka
      , expeditiousRetreat1
      , fearless
      , fearless2
      , fey1
      , fightingLessons
      , ghastlyPossession1
      , grimResolve
      , grizzled
      , gumption1
      , guts
      , guts2
      , hardboiled
      , hatchetMan
      , helpingHand
      , inquiringMind
      , inquisitive1
      , inspiringPresence
      , inspiringPresence2
      , intrepid
      , justifyTheMeans3
      , lastChance
      , lastChance3
      , leadership
      , leadership2
      , lightfooted
      , longShot
      , manualDexterity
      , manualDexterity2
      , memoriesOfAnotherLife5
      , mesmericInfluence1
      , momentum1
      , nauticalProwess
      , neitherRainNorSnow
      , nimble
      , notWithoutAFight
      , occultTheory1
      , onTheMend
      , opportunist
      , opportunist2
      , overpower
      , overpower2
      , perception
      , perception2
      , persistence1
      , planOfAction
      , predestined
      , prescient
      , promiseOfPower
      , prophesy
      , providential2
      , purified
      , quickThinking
      , quickWitted1
      , reckless
      , recklessAssault
      , resourceful
      , riseToTheOccasion
      , riseToTheOccasion3
      , runForYourLife
      , savant1
      , sayYourPrayers
      , sealOfTheElderSign5
      , selfSacrifice
      , sharpVision1
      , signumCrucis2
      , skeptic1
      , steadfast
      , strengthInNumbers1
      , strokeOfLuck2
      , strongArmed1
      , stunningBlow
      , surprisingFind1
      , surveyTheArea1
      , survivalInstinct
      , survivalInstinct2
      , takeHeart
      , takeTheInitiative
      , theEyeOfTruth5
      , theHomeFront
      , threeAces1
      , torrentOfPower
      , trueUnderstanding
      , unexpectedCourage
      , unexpectedCourage2
      , unrelenting1
      , viciousBlow
      , viciousBlow2
      , watchThis
      , watchThis3
      , wellDressed
      , wellFunded
      , whispersFromTheDeep
      ]

viciousBlow :: CardDef
viciousBlow =
  (skill "01025" "Vicious Blow" [#combat] Guardian)
    { cdCardTraits = setFromList [Practiced]
    , cdAlternateCardCodes = ["01525", "60119"]
    }

deduction :: CardDef
deduction =
  (skill "01039" "Deduction" [#intellect] Seeker)
    { cdCardTraits = setFromList [Practiced]
    , cdAlternateCardCodes = ["01539", "60219"]
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
overpower =
  (skill "01091" "Overpower" [#combat, #combat] Neutral)
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
    }

theHomeFront :: CardDef
theHomeFront =
  signature "03001"
    $ (skill "03007" "The Home Front" (replicate 4 #combat) Neutral)
      { cdCardTraits = setFromList [Practiced, Expert]
      }

resourceful :: CardDef
resourceful =
  (skill "03039" "Resourceful" [#intellect, #combat, #agility] Survivor)
    { cdCardTraits = singleton Innate
    }

sayYourPrayers :: CardDef
sayYourPrayers =
  (skill "03116" "Say Your Prayers" [#willpower, #willpower, #willpower, #willpower] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

desperateSearch :: CardDef
desperateSearch =
  (skill "03117" "Desperate Search" [#intellect, #intellect, #intellect, #intellect] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

recklessAssault :: CardDef
recklessAssault =
  (skill "03118" "Reckless Assault" [#combat, #combat, #combat, #combat] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

runForYourLife :: CardDef
runForYourLife =
  (skill "03119" "Run For Your Life" [#agility, #agility, #agility, #agility] Neutral)
    { cdCardTraits = singleton Desperate
    , cdCommitRestrictions =
        [ MaxOnePerTest
        , SelfCanCommitWhen $ InvestigatorWithRemainingSanity $ AtMost $ Static 3
        ]
    }

inspiringPresence :: CardDef
inspiringPresence =
  (skill "03228" "Inspiring Presence" [#willpower, #intellect, #combat] Guardian)
    { cdCardTraits = singleton Innate
    }

eureka :: CardDef
eureka =
  (skill "03231" "Eureka!" [#willpower, #intellect, #agility] Seeker)
    { cdCardTraits = singleton Innate
    }

watchThis :: CardDef
watchThis =
  (skill "03233" "\"Watch this!\"" [#willpower, #combat, #agility] Rogue)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyYourTest]
    }

torrentOfPower :: CardDef
torrentOfPower =
  (skill "03235" "Torrent of Power" [#wild] Mystic)
    { cdCardTraits = singleton Practiced
    }

notWithoutAFight :: CardDef
notWithoutAFight =
  (skill "03272" "\"Not without a fight!\"" [#willpower, #combat, #agility] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [SelfCanCommitWhen $ InvestigatorEngagedWith AnyEnemy]
    }

sealOfTheElderSign5 :: CardDef
sealOfTheElderSign5 =
  (skill "03312" "Seal of the Elder Sign" [#wild] Mystic)
    { cdCardTraits = setFromList [Spell, Expert]
    , cdLevel = Just 5
    }

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
    }

steadfast :: CardDef
steadfast =
  (skill "05022" "Steadfast" [#willpower, #combat] Guardian)
    { cdCardTraits = singleton Innate
    , cdOutOfPlayEffects = [InHandEffect]
    }

curiosity :: CardDef
curiosity =
  (skill "05026" "Curiosity" [#willpower, #intellect] Seeker)
    { cdCardTraits = singleton Innate
    }

cunning :: CardDef
cunning =
  (skill "05030" "Cunning" [#intellect, #agility] Rogue)
    { cdCardTraits = singleton Innate
    }

prophesy :: CardDef
prophesy =
  (skill "05034" "Prophesy" [#wild] Mystic)
    { cdCardTraits = singleton Practiced
    }

ableBodied :: CardDef
ableBodied =
  (skill "05038" "Able Bodied" [#combat, #agility] Survivor)
    { cdCardTraits = singleton Innate
    }

daring :: CardDef
daring =
  (skill "06111" "Daring" [#wild, #wild, #wild] Guardian)
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyTestWithActions [#fight, #evade]]
    }

essenceOfTheDream :: CardDef
essenceOfTheDream =
  (skill "06113" "Essence of the Dream" [#wild, #wild] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdKeywords = singleton (Keyword.Bonded 1 "06112")
    , cdLevel = Nothing
    , cdWhenDiscarded = ToBonded
    }

momentum1 :: CardDef
momentum1 =
  (skill "06115" "Momentum" [#wild] Rogue)
    { cdCardTraits = singleton Practiced
    , cdLevel = Just 1
    }

selfSacrifice :: CardDef
selfSacrifice =
  (skill "06157" "Self-Sacrifice" [] Guardian)
    { cdCardTraits = singleton Spirit
    , cdCommitRestrictions = [OnlyInvestigator $ NotYou <> colocatedWithMatch You]
    }

bruteForce1 :: CardDef
bruteForce1 =
  (skill "06166" "Brute Force" [#combat] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

threeAces1 :: CardDef
threeAces1 =
  (skill "06199" "Three Aces" [#wild] Rogue)
    { cdKeywords = singleton Keyword.Myriad
    , cdCardTraits = setFromList [Fortune, Practiced]
    , cdLevel = Just 1
    }

sharpVision1 :: CardDef
sharpVision1 =
  (skill "06204" "Sharp Vision" [#intellect] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

leadership2 :: CardDef
leadership2 =
  (skill "06235" "Leadership" [#wild] Guardian)
    { cdCardTraits = singleton Practiced
    , cdLevel = Just 2
    }

daredevil2 :: CardDef
daredevil2 =
  (skill "06240" "Daredevil" [#wild] Rogue)
    { cdCardTraits = setFromList [Fortune, Practiced]
    , cdLevel = Just 2
    }

expeditiousRetreat1 :: CardDef
expeditiousRetreat1 =
  (skill "06246" "Expeditious Retreat" [#agility] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

surprisingFind1 :: CardDef
surprisingFind1 =
  (skill "06278" "Surprising Find" [#wild] Seeker)
    { cdCardTraits = setFromList [Fortune, Research]
    , cdKeywords = singleton Keyword.Myriad
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InSearchEffect]
    }

theEyeOfTruth5 :: CardDef
theEyeOfTruth5 =
  (skill "06325" "The Eye of Truth" [#wild, #wild, #wild, #wild] Seeker)
    { cdCardTraits = setFromList [Spell, Practiced]
    , cdLevel = Just 5
    }

dreamParasite :: CardDef
dreamParasite =
  (skill "06331" "Dream Parasite" [#wildMinus, #wildMinus] Neutral)
    { cdCardTraits = singleton Curse
    , cdCardSubType = Just Weakness
    , cdCommitRestrictions = [MustBeCommittedToYourTest]
    , cdKeywords = singleton (Keyword.Bonded 3 "06330")
    , cdLevel = Nothing
    }

whispersFromTheDeep :: CardDef
whispersFromTheDeep =
  (skill "07009" "Whispers from the Deep" [#wildMinus] Neutral)
    { cdCardTraits = singleton Curse
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdOutOfPlayEffects = [InHandEffect]
    }

planOfAction :: CardDef
planOfAction =
  (skill "07024" "Plan of Action" [#wild] Seeker)
    { cdCardTraits = setFromList [Practiced]
    }

promiseOfPower :: CardDef
promiseOfPower =
  (skill "07032" "Promise of Power" [#wild, #wild, #wild, #wild] Mystic)
    { cdCardTraits = setFromList [Practiced, Cursed]
    }

predestined :: CardDef
predestined =
  (skill "07035" "Predestined" [] Survivor)
    { cdCardTraits = setFromList [Fortune, Blessed]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

beloved :: CardDef
beloved =
  (skill "07036" "Beloved" [#willpower, #agility, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Blessed]
    }

skeptic1 :: CardDef
skeptic1 =
  (skill "07115" "Skeptic" [#wild] Rogue)
    { cdCardTraits = setFromList [Practiced]
    , cdLevel = Just 1
    }

unrelenting1 :: CardDef
unrelenting1 =
  (skill "07196" "Unrelenting" [#wild] Survivor)
    { cdCardTraits = singleton Practiced
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

signumCrucis2 :: CardDef
signumCrucis2 =
  (skill "07197" "Signum Crucis" [#wild] Survivor)
    { cdCardTraits = setFromList [Practiced, Blessed]
    , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 1]
    , cdLevel = Just 2
    }

fey1 :: CardDef
fey1 =
  (skill "07222" "Fey" [#willpower, #wild, #wild] Seeker)
    { cdCardTraits = setFromList [Innate, Cursed]
    , cdLevel = Just 1
    }

justifyTheMeans3 :: CardDef
justifyTheMeans3 =
  (skill "07306" "Justify the Means" [] Rogue)
    { cdCardTraits = setFromList [Practiced, Cursed]
    , cdLevel = Just 3
    }

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

asYouWish :: CardDef
asYouWish =
  signature "09001"
    $ (skill "09002" "\"As you wish\"" [#wild, #wild, #wild] Neutral)
      { cdCardTraits = setFromList [Practiced, Expert]
      , cdCommitRestrictions = [OnlyNotYourTest]
      }

onTheMend :: CardDef
onTheMend =
  signature "09004"
    $ (skill "09006" "On the Mend" [#wild, #wild] Neutral)
      { cdCardTraits = setFromList [Innate]
      , cdCommitRestrictions = [OnlyYourTest]
      , cdWhenDiscarded = ToSetAside
      }

fightingLessons :: CardDef
fightingLessons =
  (skill "09030" "Fighting Lessons" [#combat, #agility, #wild] Guardian)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [OnlyTestWithActions [#fight, #evade]]
    , cdOutOfPlayEffects = [InHandEffect]
    }

helpingHand :: CardDef
helpingHand =
  (skill "09031" "Helping Hand" [] Guardian)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

analysis :: CardDef
analysis =
  (skill "09049" "Analysis" [#wild] Seeker)
    { cdCardTraits = setFromList [Practiced]
    }

calculatedRisk :: CardDef
calculatedRisk =
  (skill "09070" "Calculated Risk" [] Rogue)
    { cdCardTraits = setFromList [Gambit, Fated]
    , cdCommitRestrictions = [OnlyYourTest, OnlyTestDuringYourTurn, MaxOnePerTest]
    }

ghastlyPossession1 :: CardDef
ghastlyPossession1 =
  (skill "09090" "Ghastly Possession" [#wild] Mystic)
    { cdCardTraits = setFromList [Innate, Spell]
    , cdLevel = Just 1
    }

grizzled :: CardDef
grizzled =
  (skill "09101" "Grizzled" [#wild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdKeywords = setFromList [Keyword.Customizable]
    , cdOutOfPlayEffects = [InHandEffect, InDiscardEffect]
    , cdCustomizations =
        mapFromList
          [ (ChoicePlaceholder, 0)
          , (Specialist, 1)
          , (Specialist2, 2)
          , (Nemesis, 3)
          , (MythosHardened, 4)
          , (AlwaysPrepared, 5)
          ]
    }

gumption1 :: CardDef
gumption1 =
  (skill "09112" "Gumption" [] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 1
    }

purified :: CardDef
purified =
  (skill "10029" "Purified" [] Guardian)
    { cdCardTraits = setFromList [Innate, Blessed]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

strongArmed1 :: CardDef
strongArmed1 =
  (skill "10031" "Strong-Armed" [#combat, #agility] Guardian)
    { cdCardTraits = setFromList [Innate]
    , cdLevel = Just 1
    }

wellFunded :: CardDef
wellFunded =
  (skill "10051" "Well-Funded" [#wild] Seeker)
    { cdCardTraits = setFromList [Fortune]
    , cdOutOfPlayEffects = [InHandEffect]
    }

esotericMethod1 :: CardDef
esotericMethod1 =
  (skill "10055" "Esoteric Method" [#wild, #wild, #wild, #wild] Seeker)
    { cdCardTraits = setFromList [Practiced, Cursed]
    , cdLevel = Just 1
    }

diabolicalLuck :: CardDef
diabolicalLuck =
  (skill "10075" "Diabolical Luck" [#wild] Rogue)
    { cdCardTraits = setFromList [Fortune, Cursed]
    , cdOutOfPlayEffects = [InHandEffect]
    }

lightfooted :: CardDef
lightfooted =
  (skill "10076" "Lightfooted" [#agility] Rogue)
    { cdCardTraits = setFromList [Practiced, Trick]
    }

accursed :: CardDef
accursed =
  (skill "10095" "Accursed" [#wild] Mystic)
    { cdCardTraits = setFromList [Innate, Cursed]
    }

mesmericInfluence1 :: CardDef
mesmericInfluence1 =
  (skill "10096" "Mesmeric Influence" [#willpower, #wild] Mystic)
    { cdCardTraits = setFromList [Practiced]
    , cdLevel = Just 1
    }

longShot :: CardDef
longShot =
  (skill "10116" "Long Shot" [] Survivor)
    { cdCardTraits = setFromList [Practiced]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdCommitRestrictions =
        [ AnyCommitRestriction
            [ OnlyFightAgainst (EnemyAt $ oneOf [YourLocation, ConnectedLocation NotForMovement])
            , OnlyEvasionAgainst (EnemyAt $ oneOf [YourLocation, ConnectedLocation NotForMovement])
            ]
        ]
    }

persistence1 :: CardDef
persistence1 =
  (skill "10118" "Persistence" [#wild] Survivor)
    { cdCardTraits = setFromList [Practiced]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InDiscardEffect]
    }

providential2 :: CardDef
providential2 =
  (skill "10125" "Providential" [#willpower, #combat, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Blessed]
    , cdLevel = Just 2
    }

wellDressed :: CardDef
wellDressed =
  (skill "10130" "Well-Dressed" [#wild] Neutral)
    { cdCardTraits = setFromList [Practiced, Fortune]
    }

determined :: CardDef
determined =
  signature "11001"
    $ (skill "11002" "Determined" [#wild] Neutral)
      { cdCardTraits = setFromList [Innate]
      }

grimResolve :: CardDef
grimResolve =
  signature "11017"
    $ (skill "11018" "Grim Resolve" [#wild] Neutral)
      { cdCardTraits = setFromList [Innate, Developed]
      }

hardboiled :: CardDef
hardboiled =
  (skill "11025" "Hardboiled" [#combat, #wild] Guardian)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdOutOfPlayEffects = [InHandEffect]
    }

inspiringPresence2 :: CardDef
inspiringPresence2 =
  (skill "11029" "Inspiring Presence" [#willpower, #intellect, #combat, #wild] Guardian)
    { cdCardTraits = setFromList [Developed, Innate]
    , cdLevel = Just 2
    }

quickWitted1 :: CardDef
quickWitted1 =
  (skill "11042" "Quick-Witted" [#intellect, #agility] Seeker)
    { cdCardTraits = singleton Innate
    , cdKeywords = setFromList [Keyword.Myriad]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InHandEffect]
    }

crackShot :: CardDef
crackShot =
  (skill "11057" "Crack Shot" [#wild, #wild, #wild] Rogue)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions =
        [ OnlySkillTestSource
            $ SourceIsAsset
            $ AssetControlledBy You
            <> mapOneOf AssetWithTrait [Firearm, Ranged]
        ]
    }

watchThis3 :: CardDef
watchThis3 =
  (skill "11061" "\"Watch this!\"" [#wild, #wild, #wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyYourTest]
    , cdLevel = Just 3
    }

enraptured2 :: CardDef
enraptured2 =
  (skill "11077" "Enraptured" [#intellect, #wild] Mystic)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdLevel = Just 2
    , cdCommitRestrictions = [MaxOnePerTest]
    }

contemplative :: CardDef
contemplative =
  (skill "11088" "Contemplative" [] Survivor)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions =
        [ OnlySkillTest
            $ oneOf
              [ WhileParleying <> SkillTestAt (orConnected NotForMovement YourLocation)
              , WhileInvestigating (orConnected NotForMovement YourLocation)
              ]
        ]
    }

lastChance3 :: CardDef
lastChance3 =
  (skill "11093" "Last Chance" [#wild, #wild, #wild, #wild, #wild, #wild] Survivor)
    { cdCardTraits = singleton Gambit
    , cdCommitRestrictions = [OnlyCardCommittedToTest]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdLevel = Just 3
    }

confidence :: CardDef
confidence =
  (skill "11097" "Confidence" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

dreamsOfTheClay1 :: CardDef
dreamsOfTheClay1 =
  (skill "11100" "Dreams of the Clay" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate, Augury]
    , cdCommitRestrictions = [OnlySkillTestSource (SourceIsTreacheryEffect AnyTreachery)]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Artist, Clairvoyant, Dreamer, Performer]]
    , cdLevel = Just 1
    }

inquisitive1 :: CardDef
inquisitive1 =
  (skill "11101" "Inquisitive" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Assistant, Miskatonic, Scholar]]
    , cdLevel = Just 1
    }

doubleDown2 :: CardDef
doubleDown2 =
  (skill "11107" "Double Down" [#wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Criminal, Entrepreneur, Socialite]]
    , cdLevel = Just 2
    }

memoriesOfAnotherLife5 :: CardDef
memoriesOfAnotherLife5 =
  (skill "11125" "Memories of Another Life" [#wild, #wild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdDeckRestrictions = [OnlyInvestigatorWithTraits [Artist, Believer, Dreamer, Sorcerer]]
    , cdLevel = Just 5
    }

riseToTheOccasion3 :: CardDef
riseToTheOccasion3 =
  ( skill
      "51010"
      "Rise to the Occasion"
      [#wild, #wild]
      Survivor
  )
    { cdCardTraits = singleton Innate
    , cdCommitRestrictions = [OnlyYourTest, MinSkillTestValueDifference 1]
    , cdLevel = Just 3
    }

overpower2 :: CardDef
overpower2 =
  (skill "60126" "Overpower" [#combat, #combat, #combat] Guardian)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

perception2 :: CardDef
perception2 =
  (skill "60228" "Perception" [#intellect, #intellect, #intellect] Seeker)
    { cdCardTraits = setFromList [Practiced, Expert]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

anythingYouCanDoBetter :: CardDef
anythingYouCanDoBetter =
  signature "60301"
    $ ( skill
          "60302"
          "Anything You Can Do, Better"
          [#wild, #wild, #wild, #wild, #wild, #wild]
          Rogue
      )
      { cdCardTraits = setFromList [Innate, Developed]
      , cdCommitRestrictions = [OnlyYourTest]
      }

arrogance :: CardDef
arrogance =
  (skill "60303" "Arrogance" [#wildMinus] Neutral)
    { cdCardTraits = singleton Flaw
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCommitRestrictions = [MustBeCommittedToYourTest]
    }

reckless :: CardDef
reckless =
  (skill "60304" "Reckless" [] Neutral)
    { cdCardTraits = singleton Flaw
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCommitRestrictions = [OnlyYourTest, OnlyCardCommittedToTest]
    , cdOutOfPlayEffects = [InHandEffect]
    }

nimble :: CardDef
nimble =
  (skill "60317" "Nimble" [#agility] Rogue)
    { cdCardTraits = singleton Innate
    }

daredevil :: CardDef
daredevil =
  (skill "60318" "Daredevil" [#wild] Rogue)
    { cdCardTraits = setFromList [Fortune, Practiced]
    }

manualDexterity2 :: CardDef
manualDexterity2 =
  (skill "60325" "Manual Dexterity" [#agility, #agility, #agility] Rogue)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

copycat3 :: CardDef
copycat3 =
  (skill "60330" "Copycat" [#wild] Rogue)
    { cdCardTraits = singleton Gambit
    , cdLevel = Just 3
    }

prescient :: CardDef
prescient =
  (skill "60419" "Prescient" [#willpower] Mystic)
    { cdCardTraits = setFromList [Practiced, Augury]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

guts2 :: CardDef
guts2 =
  (skill "60424" "Guts" [#willpower, #willpower, #willpower] Mystic)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

neitherRainNorSnow :: CardDef
neitherRainNorSnow =
  signature "60501"
    $ (skill "60502" "Neither Rain nor Snow" [#wild, #wild, #wild] Survivor)
      { cdCardTraits = setFromList [Innate, Developed]
      }

unexpectedCourage2 :: CardDef
unexpectedCourage2 =
  (skill "60526" "Unexpected Courage" [#wild, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Developed]
    , cdCommitRestrictions = [MaxOnePerTest]
    , cdLevel = Just 2
    }

nauticalProwess :: CardDef
nauticalProwess =
  signature "07005"
    $ (skill "98014" "Nautical Prowess" [#willpower, #intellect, #wild] Neutral)
      { cdCardTraits = setFromList [Innate, Developed]
      }

dreamsOfTheDeepTheDeepGate :: CardDef
dreamsOfTheDeepTheDeepGate =
  (skill "98015" ("Dreams of the Deep" <:> "The Deep Gate") [#wildMinus, #wildMinus] Neutral)
    { cdCardTraits = setFromList [Curse]
    , cdLevel = Nothing
    , cdCardSubType = Just Weakness
    , cdOutOfPlayEffects = [InHandEffect]
    }
