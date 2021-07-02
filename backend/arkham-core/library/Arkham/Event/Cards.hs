module Arkham.Event.Cards where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.ClassSymbol
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

event :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
event cardCode name cost classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Just (StaticCost cost)
  , cdLevel = 0
  , cdCardType = EventType
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
  }

allEventCards :: HashMap CardCode CardDef
allEventCards = mapFromList
  [ ("01010", onTheLam)
  , ("01013", darkMemory)
  , ("01022", evidence)
  , ("01023", dodge)
  , ("01024", dynamiteBlast)
  , ("01026", extraAmmunition1)
  , ("01036", mindOverMatter)
  , ("01037", workingAHunch)
  , ("01038", barricade)
  , ("01043", crypticResearch4)
  , ("01050", elusive)
  , ("01051", backstab)
  , ("01052", sneakAttack)
  , ("01057", hotStreak4)
  , ("01064", drawnToTheFlame)
  , ("01065", wardOfProtection)
  , ("01066", blindingLight)
  , ("01068", mindWipe1)
  , ("01069", blindingLight2)
  , ("01078", cunningDistraction)
  , ("01079", lookWhatIFound)
  , ("01080", lucky)
  , ("01083", closeCall2)
  , ("01084", lucky2)
  , ("01085", willToSurvive4)
  , ("01088", emergencyCache)
  , ("02008", searchForTheTruth)
  , ("02017", taunt)
  , ("02018", teamwork)
  , ("02019", taunt2)
  , ("02022", shortcut)
  , ("02023", seekingAnswers)
  , ("02025", thinkOnYourFeet)
  , ("02031", bindMonster2)
  , ("02034", baitAndSwitch)
  , ("02105", emergencyAid)
  , ("02107", iveGotAPlan)
  , ("02109", contraband)
  , ("02111", delveTooDeep)
  , ("03022", letMeHandleThis)
  , ("04149", secondWind)
  , ("05317", bloodRite)
  , ("06110", firstWatch)
  , ("06023", astoundingRevelation)
  , ("50002", dynamiteBlast2)
  , ("50004", barricade3)
  , ("50006", hotStreak2)
  , ("50008", mindWipe3)
  , ("51005", contraband2)
  , ("60130", taunt3)
  , ("60225", iveGotAPlan2)
  ]

onTheLam :: CardDef
onTheLam = (event "01010" "On the Lam" 1 Neutral)
  { cdCardTraits = setFromList [Tactic]
  , cdSkills = [SkillIntellect, SkillAgility, SkillWild, SkillWild]
  , cdFast = True
  , cdWindows = setFromList [AfterTurnBegins You, DuringTurn You]
  }

darkMemory :: CardDef
darkMemory = (event "01013" "Dark Memory" 2 Neutral)
  { cdCardTraits = setFromList [Spell]
  , cdWeakness = True
  }

evidence :: CardDef
evidence = (event "01022" "Evidence!" 1 Guardian)
  { cdSkills = [SkillIntellect, SkillIntellect]
  , cdCardTraits = setFromList [Insight]
  , cdFast = True
  , cdWindows = setFromList [WhenEnemyDefeated You]
  }

dodge :: CardDef
dodge = (event "01023" "Dodge" 1 Guardian)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Tactic]
  , cdFast = True
  , cdWindows = setFromList [WhenEnemyAttacks InvestigatorAtYourLocation]
  }

dynamiteBlast :: CardDef
dynamiteBlast = (event "01024" "Dynamite Blast" 5 Guardian)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Tactic]
  }

extraAmmunition1 :: CardDef
extraAmmunition1 = (event "01026" "Extra Ammunition" 2 Guardian)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Supply]
  , cdLevel = 1
  }

mindOverMatter :: CardDef
mindOverMatter = (event "01036" "Mind over Matter" 1 Seeker)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = setFromList [Insight]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

workingAHunch :: CardDef
workingAHunch = (event "01037" "Working a Hunch" 2 Seeker)
  { cdSkills = [SkillIntellect, SkillIntellect]
  , cdCardTraits = setFromList [Insight]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

barricade :: CardDef
barricade = (event "01038" "Barricade" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Insight, Tactic]
  }

crypticResearch4 :: CardDef
crypticResearch4 = (event "01043" "Cryptic Research" 0 Seeker)
  { cdCardTraits = setFromList [Insight]
  , cdLevel = 4
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

elusive :: CardDef
elusive = (event "01050" "Elusive" 2 Rogue)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Tactic]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

backstab :: CardDef
backstab = (event "01051" "Backstab" 3 Rogue)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = setFromList [Tactic]
  , cdAction = Just Action.Fight
  }

sneakAttack :: CardDef
sneakAttack = (event "01052" "Sneak Attack" 2 Rogue)
  { cdSkills = [SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Tactic]
  }

hotStreak4 :: CardDef
hotStreak4 = (event "01057" "Hot Streak" 2 Rogue)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Fortune]
  , cdLevel = 4
  }

drawnToTheFlame :: CardDef
drawnToTheFlame = (event "01064" "Drawn to the Flame" 0 Mystic)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = setFromList [Insight]
  }

wardOfProtection :: CardDef
wardOfProtection = (event "01065" "Ward of Protection" 1 Mystic)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdFast = True
  , cdWindows = setFromList [WhenDrawTreachery You]
  }

blindingLight :: CardDef
blindingLight = (event "01066" "Blinding Light" 2 Mystic)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Spell]
  , cdAction = Just Action.Evade
  }

mindWipe1 :: CardDef
mindWipe1 = (event "01068" "Mind Wipe" 1 Mystic)
  { cdSkills = [SkillWillpower, SkillCombat]
  , cdCardTraits = setFromList [Spell]
  , cdLevel = 1
  , cdFast = True
  , cdWindows = setFromList [AnyPhaseBegins]
  }

blindingLight2 :: CardDef
blindingLight2 = (event "01069" "Blinding Light" 1 Mystic)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Spell]
  , cdAction = Just Action.Evade
  , cdLevel = 2
  }

cunningDistraction :: CardDef
cunningDistraction =
  (event "01078" "Cunning Distraction" 5 Survivor)
    { cdSkills = [SkillIntellect, SkillWild]
    , cdCardTraits = setFromList [Tactic]
    , cdAction = Just Action.Evade
    }

lookWhatIFound :: CardDef
lookWhatIFound =
  (event "01079" "\"Look what I found!\"" 2 Survivor)
    { cdSkills = [SkillIntellect, SkillIntellect]
    , cdCardTraits = setFromList [Fortune]
    , cdFast = True
    , cdWindows = setFromList
      [ AfterFailInvestigationSkillTest You n | n <- [0 .. 2] ]
    }

lucky :: CardDef
lucky = (event "01080" "Lucky!" 1 Survivor)
  { cdCardTraits = setFromList [Fortune]
  , cdFast = True
  , cdWindows = setFromList [WhenWouldFailSkillTest You]
  }

closeCall2 :: CardDef
closeCall2 = (event "01083" "Close Call" 2 Survivor)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = setFromList [Fortune]
  , cdFast = True
  , cdWindows = mempty -- We handle this via behavior
  , cdLevel = 2
  }

lucky2 :: CardDef
lucky2 = (event "01084" "Lucky!" 1 Survivor)
  { cdCardTraits = setFromList [Fortune]
  , cdFast = True
  , cdWindows = setFromList [WhenWouldFailSkillTest You]
  , cdLevel = 2
  }

willToSurvive4 :: CardDef
willToSurvive4 = (event "01085" "Will to Survive" 4 Survivor)
  { cdSkills = [SkillCombat, SkillWild]
  , cdCardTraits = setFromList [Spirit]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  , cdLevel = 4
  }

emergencyCache :: CardDef
emergencyCache = (event "01088" "Emergency Cache" 0 Neutral)
  { cdCardTraits = setFromList [Supply]
  }

searchForTheTruth :: CardDef
searchForTheTruth =
  (event "02008" "Search for the Truth" 1 Neutral)
    { cdSkills = [SkillIntellect, SkillIntellect, SkillWild]
    , cdCardTraits = setFromList [Insight]
    }

taunt :: CardDef
taunt = (event "02017" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  , cdSkills = [SkillWillpower, SkillCombat]
  }

teamwork :: CardDef
teamwork = (event "02018" "Teamwork" 0 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdSkills = [SkillWild]
  }

taunt2 :: CardDef
taunt2 = (event "02019" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  , cdSkills = [SkillWillpower, SkillCombat, SkillAgility]
  }

shortcut :: CardDef
shortcut = (event "02022" "Shortcut" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

seekingAnswers :: CardDef
seekingAnswers = (event "02023" "Seeking Answers" 1 Seeker)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = singleton Insight
  }

thinkOnYourFeet :: CardDef
thinkOnYourFeet = (event "02025" "Think on Your Feet" 1 Rogue)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = singleton Trick
  , cdFast = True
  , cdWindows = setFromList [WhenEnemySpawns YourLocation []]
  }

bindMonster2 :: CardDef
bindMonster2 = (event "02031" "Bind Monster" 3 Mystic)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = singleton Spell
  , cdAction = Just Action.Evade
  , cdLevel = 2
  }

baitAndSwitch :: CardDef
baitAndSwitch = (event "02034" "Bait and Switch" 1 Survivor)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Trick]
  , cdAction = Just Action.Evade
  }

emergencyAid :: CardDef
emergencyAid = (event "02105" "Emergency Aid" 2 Guardian)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Insight, Science]
  }

iveGotAPlan :: CardDef
iveGotAPlan = (event "02107" "\"I've Got a Plan!\"" 3 Seeker)
  { cdSkills = [SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Insight, Tactic]
  }

contraband :: CardDef
contraband = (event "02109" "Contraband" 4 Rogue)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = setFromList [Supply, Illicit]
  }

delveTooDeep :: CardDef
delveTooDeep = (event "02111" "Delve Too Deep" 1 Mystic)
  { cdCardTraits = setFromList [Insight]
  , cdVictoryPoints = Just 1
  }

letMeHandleThis :: CardDef
letMeHandleThis =
  (event "03022" "\"Let me handle this!\"" 0 Guardian)
    { cdSkills = [SkillWillpower, SkillCombat]
    , cdCardTraits = setFromList [Spirit]
    , cdFast = True
    , cdWindows = mempty -- We handle this via behavior
    }

secondWind :: CardDef
secondWind = (event "04149" "Second Wind" 1 Guardian)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Spirit, Bold]
  , cdFast = True -- not fast
  , cdWindows = mempty -- handle via behavior since must be first action
  }

bloodRite :: CardDef
bloodRite = (event "05317" "Blood Rite" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Spell]
  }

firstWatch :: CardDef
firstWatch = (event "06110" "First Watch" 1 Guardian)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Tactic]
  , cdFast = True
  , cdWindows = setFromList [WhenAllDrawEncounterCard]
  }

astoundingRevelation :: CardDef
astoundingRevelation =
  (event "06023" "Astounding Revelation" 0 Seeker)
    { cdSkills = [SkillIntellect]
    , cdCardTraits = setFromList [Research]
    , cdFast = True
    , cdWindows = mempty -- cannot be played
    }

dynamiteBlast2 :: CardDef
dynamiteBlast2 = (event "50002" "Dynamite Blast" 4 Guardian)
  { cdSkills = [SkillWillpower, SkillCombat]
  , cdCardTraits = setFromList [Tactic]
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdLevel = 2
  }

barricade3 :: CardDef
barricade3 = (event "50004" "Barricade" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdLevel = 3
  }

hotStreak2 :: CardDef
hotStreak2 = (event "50006" "Hot Streak" 5 Rogue)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Fortune]
  , cdLevel = 2
  }

mindWipe3 :: CardDef
mindWipe3 = (event "50008" "Mind Wipe" 1 Mystic)
  { cdSkills = [SkillWillpower, SkillCombat]
  , cdCardTraits = setFromList [Spell]
  , cdLevel = 3
  , cdFast = True
  , cdWindows = setFromList [AnyPhaseBegins]
  }

contraband2 :: CardDef
contraband2 = (event "51005" "Contraband" 3 Rogue)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillIntellect]
  , cdCardTraits = setFromList [Supply, Illicit]
  , cdLevel = 2
  }

taunt3 :: CardDef
taunt3 = (event "60130" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFast = True
  , cdWindows = setFromList [FastPlayerWindow]
  , cdSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillAgility]
  }

iveGotAPlan2 :: CardDef
iveGotAPlan2 = (event "60225" "\"I've Got a Plan!\"" 2 Seeker)
  { cdSkills = [SkillIntellect, SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Insight, Tactic]
  }
