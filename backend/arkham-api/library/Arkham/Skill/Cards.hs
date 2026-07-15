module Arkham.Skill.Cards (module Arkham.Skill.Cards, module X) where

import Arkham.Skill.CardDefs.Base as X
import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Skill.CardDefs.NightOfTheZealot as X
import Arkham.Skill.CardDefs.TheDunwichLegacy as X
import Arkham.Skill.CardDefs.ThePathToCarcosa as X
import Arkham.Skill.CardDefs.TheForgottenAge as X
import Arkham.Skill.CardDefs.TheCircleUndone as X
import Arkham.Skill.CardDefs.TheDreamEaters as X
import Arkham.Skill.CardDefs.TheInnsmouthConspiracy as X
import Arkham.Skill.CardDefs.EdgeOfTheEarth as X
import Arkham.Skill.CardDefs.TheScarletKeys as X
import Arkham.Skill.CardDefs.TheFeastOfHemlockVale as X
import Arkham.Skill.CardDefs.TheDrownedCity as X
import Arkham.Skill.CardDefs.Core2026 as X
import Arkham.Skill.CardDefs.ReturnTo as X
import Arkham.Skill.CardDefs.Standalone as X
import Arkham.Skill.CardDefs.Promo as X

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Prelude

allPlayerSkillCards :: Map CardCode CardDef
allPlayerSkillCards = (Homebrew.playerSkillsMap <>) $
  mapFromList
    $ concatMap
      toCardCodePairs
      [ ableBodied
      , accursed
      , allIn5
      , analysis
      , anythingYouCanDoBetter
      , knowTheExit
      , knowTheLine
      , knowTheScene
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
      , doOrDie
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
      , onTheBrink
      , onTheBrink2
      , onTheMend
      , opportunist
      , opportunist2
      , outOfSight
      , outOfSight3
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
      , rough1
      , runForYourLife
      , savant1
      , sayYourPrayers
      , sealOfTheElderSign5
      , selfSacrifice
      , sharpVision1
      , signumCrucis2
      , skeptic1
      , slippery
      , soulLink
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
      , theHemlockCurse
      , theHomeFront
      , threeAces1
      , timelyIntervention
      , timelyIntervention3
      , torrentOfPower
      , trueUnderstanding
      , unexpectedCourage
      , unexpectedCourage2
      , unrelenting1
      , viciousBlow
      , viciousBlow2
      , adaptAndOvercome
      , armedToTheTeeth
      , indomitable3
      , establishMotive
      , literaryAnalysis
      , bloodCurse
      , cosmicGuidance
      , eldritchWhispers1
      , bloodCurse3
      , watchThis
      , watchThis3
      , wellDressed
      , wellFunded
      , whispersFromTheDeep
      , easyStreet
      , outTheDoor
      , outTheDoor1
      , contingency3
      ]
