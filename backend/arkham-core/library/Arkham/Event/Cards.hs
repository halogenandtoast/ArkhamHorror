module Arkham.Event.Cards where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.Asset.Uses as Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Card.Cost
import Arkham.Types.ClassSymbol
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Name
import qualified Arkham.Types.Restriction as Restriction
import Arkham.Types.SkillType
import Arkham.Types.Trait

event :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
event cardCode name cost classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Just (StaticCost cost)
  , cdLevel = 0
  , cdCardType = EventType
  , cdWeakness = False
  , cdClassSymbol = Just classSymbol
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdAction = Nothing
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdPlayRestrictions = Nothing
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  , cdExceptional = False
  , cdUses = Uses.NoUses
  }

allPlayerEventCards :: HashMap CardCode CardDef
allPlayerEventCards = mapFromList $ map
  (toCardCode &&& id)
  [ aChanceEncounter
  , aceInTheHole3
  , astoundingRevelation
  , backstab
  , baitAndSwitch
  , barricade
  , barricade3
  , bindMonster2
  , blindingLight
  , blindingLight2
  , bloodRite
  , buryThemDeep
  , closeCall2
  , contraband
  , contraband2
  , crypticResearch4
  , cunningDistraction
  , darkMemory
  , decipheredReality5
  , delveTooDeep
  , dodge
  , drawnToTheFlame
  , dynamiteBlast
  , dynamiteBlast2
  , elusive
  , emergencyAid
  , emergencyCache
  , emergencyCache2
  , everVigilant1
  , evidence
  , exposeWeakness1
  , extraAmmunition1
  , firstWatch
  , flare1
  , hotStreak2
  , hotStreak4
  , hypnoticGaze
  , ifItBleeds
  , imOuttaHere
  , improvisation
  , iveGotAPlan
  , iveGotAPlan2
  , iveHadWorse4
  , letMeHandleThis
  , lookWhatIFound
  , lucky
  , lucky2
  , lure1
  , mindOverMatter
  , mindWipe1
  , mindWipe3
  , momentOfRespite3
  , monsterSlayer5
  , moonlightRitual
  , noStoneUnturned
  , onTheLam
  , oops
  , preparedForTheWorst
  , preposterousSketches
  , preposterousSketches2
  , searchForTheTruth
  , secondWind
  , seekingAnswers
  , shortcut
  , sleightOfHand
  , sneakAttack
  , standTogether3
  , sureGamble3
  , taunt
  , taunt2
  , taunt3
  , teamwork
  , thePaintedWorld
  , thinkOnYourFeet
  , wardOfProtection
  , wardOfProtection5
  , willToSurvive3
  , workingAHunch
  ]

onTheLam :: CardDef
onTheLam = (event "01010" "On the Lam" 1 Neutral)
  { cdCardTraits = setFromList [Tactic]
  , cdSkills = [SkillIntellect, SkillAgility, SkillWild, SkillWild]
  , cdFastWindow = Just (AfterTurnBegins You)
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
  , cdFastWindow = Just (EnemyDefeated After You AnyEnemy)
  }

dodge :: CardDef
dodge = (event "01023" "Dodge" 1 Guardian)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just (EnemyAttacks When InvestigatorAtYourLocation AnyEnemy)
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
  , cdFastWindow = Just (DuringTurn You)
  }

workingAHunch :: CardDef
workingAHunch = (event "01037" "Working a Hunch" 2 Seeker)
  { cdSkills = [SkillIntellect, SkillIntellect]
  , cdCardTraits = setFromList [Insight]
  , cdFastWindow = Just (DuringTurn You)
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
  , cdFastWindow = Just (DuringTurn You)
  }

elusive :: CardDef
elusive = (event "01050" "Elusive" 2 Rogue)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = singleton Tactic
  , cdFastWindow = Just $ DuringTurn You
  , cdPlayRestrictions = Just $ Restriction.AnyRestriction
    [ Restriction.EnemyExists EnemyEngagedWithYou
    , Restriction.LocationExists
    $ RevealedLocation
    <> LocationWithoutEnemies
    <> NotYourLocation
    ]
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
  , cdPlayRestrictions = Just
    $ Restriction.EnemyExists (EnemyAtYourLocation <> ExhaustedEnemy)
  }

sureGamble3 :: CardDef
sureGamble3 = (event "01056" "Sure Gamble" 2 Rogue)
  { cdCardTraits = setFromList [Fortune, Insight]
  , cdFastWindow = Just (RevealChaosToken When You WithNegativeModifier)
  , cdLevel = 3
  }

hotStreak4 :: CardDef
hotStreak4 = (event "01057" "Hot Streak" 3 Rogue)
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
  , cdFastWindow = Just (DrawCard When You NonWeaknessTreachery)
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
  , cdFastWindow = Just (PhaseBegins After AnyPhase)
  , cdPlayRestrictions = Just
    (Restriction.EnemyExists $ EnemyAtYourLocation <> NonEliteEnemy)
  }

blindingLight2 :: CardDef
blindingLight2 = (event "01069" "Blinding Light" 1 Mystic)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Spell]
  , cdAction = Just Action.Evade
  , cdLevel = 2
  }

cunningDistraction :: CardDef
cunningDistraction = (event "01078" "Cunning Distraction" 5 Survivor)
  { cdSkills = [SkillWillpower, SkillWild]
  , cdCardTraits = setFromList [Tactic]
  , cdAction = Just Action.Evade
  }

lookWhatIFound :: CardDef
lookWhatIFound = (event "01079" "\"Look what I found!\"" 2 Survivor)
  { cdSkills = [SkillIntellect, SkillIntellect]
  , cdCardTraits = singleton Fortune
  , cdFastWindow =
    Just
    $ SkillTestResult After You WhileInvestigating
    $ FailureResult
    $ LessThan
    $ Static 3
  }

lucky :: CardDef
lucky = (event "01080" "Lucky!" 1 Survivor)
  { cdCardTraits = setFromList [Fortune]
  , cdFastWindow = Just
    (WhenWouldHaveSkillTestResult You AnySkillTest $ FailureResult AnyValue)
  }

closeCall2 :: CardDef
closeCall2 = (event "01083" "Close Call" 2 Survivor)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = setFromList [Fortune]
  , cdFastWindow = Just
    (EnemyEvaded After Anyone (EnemyAtYourLocation <> NonWeaknessEnemy))
  , cdLevel = 2
  }

lucky2 :: CardDef
lucky2 = (event "01084" "Lucky!" 1 Survivor)
  { cdCardTraits = setFromList [Fortune]
  , cdFastWindow = Just
    (WhenWouldHaveSkillTestResult You AnySkillTest $ FailureResult AnyValue)
  , cdLevel = 2
  }

willToSurvive3 :: CardDef
willToSurvive3 = (event "01085" "Will to Survive" 4 Survivor)
  { cdSkills = [SkillCombat, SkillWild]
  , cdCardTraits = setFromList [Spirit]
  , cdFastWindow = Just (DuringTurn You)
  , cdLevel = 3
  }

emergencyCache :: CardDef
emergencyCache = (event "01088" "Emergency Cache" 0 Neutral)
  { cdCardTraits = setFromList [Supply]
  }

searchForTheTruth :: CardDef
searchForTheTruth = (event "02008" "Search for the Truth" 1 Neutral)
  { cdSkills = [SkillIntellect, SkillIntellect, SkillWild]
  , cdCardTraits = setFromList [Insight]
  }

taunt :: CardDef
taunt = (event "02017" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just (DuringTurn You)
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
  , cdFastWindow = Just (DuringTurn You)
  , cdSkills = [SkillWillpower, SkillCombat, SkillAgility]
  , cdLevel = 2
  }

shortcut :: CardDef
shortcut = (event "02022" "Shortcut" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdFastWindow = Just (DuringTurn You)
  , cdPlayRestrictions = Just
    (Restriction.LocationExists AccessibleLocation
    <> Restriction.InvestigatorExists
         (InvestigatorCanMove <> InvestigatorAtYourLocation)
    )
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
  , cdFastWindow = Just (WhenEnemySpawns YourLocation AnyEnemy)
  , cdPlayRestrictions = Just
    (Restriction.LocationExists AccessibleLocation
    <> Restriction.InvestigatorExists (You <> InvestigatorCanMove)
    )
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
  , cdPlayRestrictions = Just $ Restriction.AnyRestriction
    [ Restriction.AssetExists
      (AssetOwnedBy InvestigatorAtYourLocation
      <> AssetWithDamage
      <> AssetWithTrait Ally
      )
    , Restriction.InvestigatorExists
      (InvestigatorAtYourLocation <> InvestigatorWithDamage)
    ]
  }

iveGotAPlan :: CardDef
iveGotAPlan = (event "02107" "\"I've got a plan!\"" 3 Seeker)
  { cdSkills = [SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Insight, Tactic]
  }

contraband :: CardDef
contraband = (event "02109" "Contraband" 4 Rogue)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = setFromList [Supply, Illicit]
  , cdPlayRestrictions = Just $ Restriction.AssetExists
    (AssetOwnedBy InvestigatorAtYourLocation
    <> AssetOneOf [AssetWithUses Uses.Ammo, AssetWithUses Uses.Supply]
    )
  }

delveTooDeep :: CardDef
delveTooDeep = (event "02111" "Delve Too Deep" 1 Mystic)
  { cdCardTraits = setFromList [Insight]
  , cdVictoryPoints = Just 1
  }

oops :: CardDef
oops = (event "02113" "Oops!" 2 Survivor)
  { cdSkills = [SkillCombat, SkillCombat]
  , cdCardTraits = singleton Fortune
  , cdFastWindow =
    Just
    $ SkillTestResult After You WhileAttackingAnEnemy
    $ FailureResult
    $ LessThan
    $ Static 3
  }

flare1 :: CardDef
flare1 = (event "02115" "Flare" 2 Survivor)
  { cdSkills = [SkillWild]
  , cdCardTraits = singleton Tactic
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdLevel = 1
  }

standTogether3 :: CardDef
standTogether3 = (event "02148" "Stand Together" 0 Guardian)
  { cdSkills = [SkillWillpower, SkillWillpower]
  , cdCardTraits = singleton Spirit
  , cdPlayRestrictions = Just Restriction.AnotherInvestigatorInSameLocation
  , cdLevel = 3
  }

imOuttaHere :: CardDef
imOuttaHere = (event "02151" "\"I'm outta here!\"" 0 Rogue)
  { cdSkills = [SkillAgility, SkillAgility]
  , cdCardTraits = setFromList [Trick, Spirit]
  , cdPlayRestrictions = Just Restriction.ScenarioCardHasResignAbility
  }

hypnoticGaze :: CardDef
hypnoticGaze = (event "02153" "Hypnotic Gaze" 3 Mystic)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = singleton Spell
  , cdFastWindow = Just (EnemyAttacks When InvestigatorAtYourLocation AnyEnemy)
  }

lure1 :: CardDef
lure1 = (event "02156" "Lure" 1 Survivor)
  { cdSkills = [SkillAgility, SkillAgility]
  , cdCardTraits = singleton Trick
  , cdLevel = 1
  }

preparedForTheWorst :: CardDef
preparedForTheWorst = (event "02184" "Prepared for the Worst" 1 Guardian)
  { cdSkills = [SkillIntellect, SkillCombat]
  , cdCardTraits = singleton Tactic
  }

preposterousSketches :: CardDef
preposterousSketches = (event "02186" "Preposterous Sketches" 2 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = singleton Insight
  , cdPlayRestrictions = Just Restriction.ClueOnLocation
  }

emergencyCache2 :: CardDef
emergencyCache2 = (event "02194" "Emergency Cache" 0 Neutral)
  { cdCardTraits = setFromList [Supply]
  , cdLevel = 2
  }

ifItBleeds :: CardDef
ifItBleeds = (event "02225" "\"If it bleeds...\"" 1 Guardian)
  { cdSkills = [SkillWillpower, SkillCombat]
  , cdFastWindow = Just (EnemyDefeated After You $ EnemyWithTrait Monster)
  }

exposeWeakness1 :: CardDef
exposeWeakness1 = (event "02228" "Expose Weakness" 0 Seeker)
  { cdSkills = [SkillIntellect, SkillCombat, SkillCombat]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just FastPlayerWindow
  , cdPlayRestrictions = Just $ Restriction.EnemyExists EnemyAtYourLocation
  , cdLevel = 1
  }

iveHadWorse4 :: CardDef
iveHadWorse4 = (event "02261" "\"I've had worse…\"" 0 Guardian)
  { cdSkills = [SkillWillpower, SkillWillpower, SkillAgility]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just (DealtDamageOrHorror You)
  , cdLevel = 4
  }

aceInTheHole3 :: CardDef
aceInTheHole3 = (event "02266" "Ace in the Hole" 0 Rogue)
  { cdCardTraits = singleton Trick
  , cdFastWindow = Just (DuringTurn You)
  , cdLevel = 3
  , cdExceptional = True
  }

moonlightRitual :: CardDef
moonlightRitual = (event "02267" "Moonlight Ritual" 0 Mystic)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Spell, Insight]
  , cdPlayRestrictions = Just Restriction.OwnCardWithDoom
  }

aChanceEncounter :: CardDef
aChanceEncounter = (event "02270" "A Chance Encounter" 1 Survivor)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = singleton Fortune
  , cdPlayRestrictions = Just
    $ Restriction.ReturnableCardInDiscard Restriction.AnyPlayerDiscard [Ally]
  }

momentOfRespite3 :: CardDef
momentOfRespite3 = (event "02273" "Moment of Respite" 3 Neutral)
  { cdSkills = [SkillWillpower, SkillWillpower]
  , cdCardTraits = singleton Spirit
  , cdPlayRestrictions = Just $ Restriction.NoEnemyExists EnemyAtYourLocation
  , cdLevel = 3
  }

monsterSlayer5 :: CardDef
monsterSlayer5 = (event "02300" "Monster Slayer" 1 Guardian)
  { cdSkills = [SkillCombat, SkillWild]
  , cdCardTraits = singleton Spirit
  , cdAction = Just Action.Fight
  , cdLevel = 5
  }

decipheredReality5 :: CardDef
decipheredReality5 = (event "02303" "Deciphered Reality" 4 Seeker)
  { cdSkills = [SkillIntellect, SkillIntellect, SkillWillpower]
  , cdCardTraits = singleton Insight
  , cdAction = Just Action.Investigate
  , cdLevel = 5
  }

wardOfProtection5 :: CardDef
wardOfProtection5 = (event "02307" "Ward of Protection" 1 Mystic)
  { cdSkills = [SkillWillpower, SkillWild]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdFastWindow = Just $ DrawCard When You NonWeaknessTreachery
  , cdLevel = 5
  }

thePaintedWorld :: CardDef
thePaintedWorld = (event "03012" "The Painted World" 0 Neutral)
  { cdSkills = [SkillWillpower, SkillAgility, SkillWild]
  , cdCardTraits = singleton Spell
  , cdFastWindow = Just
    (PlayerHasPlayableCard $ CardIsBeneathInvestigator You <> BasicCardMatch
      (NonExceptional <> EventCard)
    )
  , cdCost = Nothing
  }

buryThemDeep :: CardDef
buryThemDeep = (event "03016" "Bury Them Deep" 0 Neutral)
  { cdSkills = [SkillWillpower, SkillCombat, SkillWild]
  , cdCardTraits = singleton Task
  , cdFastWindow = Just
    (EnemyDefeated After Anyone $ NonEliteEnemy <> EnemyAtYourLocation)
  , cdVictoryPoints = Just 1
  }

improvisation :: CardDef
improvisation = (event "03018" "Improvisation" 0 Neutral)
  { cdSkills = [SkillWild, SkillWild]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just (DuringTurn You)
  }

letMeHandleThis :: CardDef
letMeHandleThis = (event "03022" "\"Let me handle this!\"" 0 Guardian)
  { cdSkills = [SkillWillpower, SkillCombat]
  , cdCardTraits = setFromList [Spirit]
  , cdFastWindow = Just $ DrawCard After NotYou $ NonPeril <> CardWithOneOf
    (map CardWithType encounterCardTypes)
  }

everVigilant1 :: CardDef
everVigilant1 = (event "03023" "Ever Vigilant" 0 Guardian)
  { cdSkills = [SkillIntellect, SkillIntellect]
  , cdCardTraits = singleton Tactic
  , cdLevel = 1
  , cdPlayRestrictions = Just
    (Restriction.PlayableCardExists $ BasicCardMatch AssetCard <> InHandOf You)
  }

noStoneUnturned :: CardDef
noStoneUnturned = (event "03026" "No Stone Unturned" 2 Seeker)
  { cdSkills = [SkillWild]
  , cdCardTraits = singleton Insight
  }

sleightOfHand :: CardDef
sleightOfHand = (event "03029" "Sleight of Hand" 1 Rogue)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = singleton Trick
  , cdFastWindow = Just $ DuringTurn You
  , cdPlayRestrictions = Just
    (Restriction.PlayableCardExists $ BasicCardMatch (CardWithTrait Item))
  }

secondWind :: CardDef
secondWind = (event "04149" "Second Wind" 1 Guardian)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Spirit, Bold]
  , cdPlayRestrictions = Just Restriction.FirstAction
  }

bloodRite :: CardDef
bloodRite = (event "05317" "Blood-Rite" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Spell]
  }

firstWatch :: CardDef
firstWatch = (event "06110" "First Watch" 1 Guardian)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
  }

astoundingRevelation :: CardDef
astoundingRevelation = (event "06023" "Astounding Revelation" 0 Seeker)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Research]
  , cdCost = Nothing
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
  , cdFastWindow = Just $ PhaseBegins After AnyPhase
  }

preposterousSketches2 :: CardDef
preposterousSketches2 = (event "51003" "Preposterous Sketches" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = singleton Insight
  , cdPlayRestrictions = Just Restriction.ClueOnLocation
  , cdLevel = 2
  }

contraband2 :: CardDef
contraband2 = (event "51005" "Contraband" 3 Rogue)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillIntellect]
  , cdCardTraits = setFromList [Supply, Illicit]
  , cdLevel = 2
  , cdPlayRestrictions = Just $ Restriction.AssetExists
    (AssetOwnedBy InvestigatorAtYourLocation
    <> AssetOneOf [AssetWithUseType Uses.Ammo, AssetWithUseType Uses.Supply]
    )
  }

taunt3 :: CardDef
taunt3 = (event "60130" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just $ DuringTurn You
  , cdSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillAgility]
  , cdLevel = 3
  }

iveGotAPlan2 :: CardDef
iveGotAPlan2 = (event "60225" "\"I've got a plan!\"" 2 Seeker)
  { cdSkills = [SkillIntellect, SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdLevel = 2
  }
