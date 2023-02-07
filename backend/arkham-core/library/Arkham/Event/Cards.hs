module Arkham.Event.Cards where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Asset.Uses qualified as Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.Criteria qualified as Criteria
import Arkham.Damage
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Modifier ( ModifierType (..) )
import Arkham.Name
import Arkham.Phase
import Arkham.SkillType ()
import Arkham.Source
import Arkham.Strategy
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Trait

event :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
event cardCode name cost classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Just (StaticCost cost)
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardType = EventType
  , cdCardSubType = Nothing
  , cdClassSymbols = singleton classSymbol
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdRevealedCardTraits = mempty
  , cdKeywords = mempty
  , cdFastWindow = Nothing
  , cdActions = []
  , cdRevelation = False
  , cdVictoryPoints = Nothing
  , cdVengeancePoints = Nothing
  , cdCriteria = Nothing
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
  , cdUses = Uses.NoUses
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
  }

allPlayerEventCards :: HashMap CardCode CardDef
allPlayerEventCards = mapFromList $ concatMap
  toCardCodePairs
  [ aChanceEncounter
  , aChanceEncounter2
  , aTestOfWill
  , aTestOfWill1
  , aTestOfWill2
  , aceInTheHole3
  , againstAllOdds2
  , ambush1
  , anatomicalDiagrams
  , astoundingRevelation
  , astralTravel
  , alterFate3
  , backstab
  , baitAndSwitch
  , barricade
  , barricade3
  , bindMonster2
  , blindingLight
  , blindingLight2
  , bloodEclipse1
  , bloodEclipse3
  , bloodRite
  , burningTheMidnightOil
  , buryThemDeep
  , callingInFavors
  , cheapShot
  , cheatDeath5
  , cleanThemOut
  , closeCall2
  , contraband
  , contraband2
  , coupDeGrace
  , counterpunch
  , counterpunch2
  , counterspell2
  , crypticResearch4
  , crypticWritings
  , crypticWritings2
  , cunningDistraction
  , customAmmunition3
  , daringManeuver
  , darkInsight
  , darkMemory
  , darkPact
  , darkProphecy
  , decipheredReality5
  , delayTheInevitable
  , delveTooDeep
  , denyExistence
  , devilsLuck
  , dodge
  , drawnToTheFlame
  , dumbLuck
  , dumbLuck2
  , dynamiteBlast
  , dynamiteBlast2
  , dynamiteBlast3
  , eatLead2
  , eavesdrop
  , eideticMemory3
  , elusive
  , emergencyAid
  , emergencyCache
  , emergencyCache2
  , emergencyCache3
  , everVigilant1
  , evidence
  , evidence1
  , exposeWeakness1
  , exposeWeakness3
  , extensiveResearch
  , extraAmmunition1
  , fightOrFlight
  , firstWatch
  , flare1
  , forewarned1
  , galvanize1
  , getOverHere
  , getOverHere2
  , glimpseTheUnthinkable1
  , glory
  , gritYourTeeth
  , guidance
  , heroicRescue
  , hidingSpot
  , hotStreak2
  , hotStreak4
  , hypnoticGaze
  , ifItBleeds
  , illSeeYouInHell
  , imDoneRunnin
  , imOuttaHere
  , impromptuBarrier
  , improvisation
  , improvisedWeapon
  , infighting3
  , iveGotAPlan
  , iveGotAPlan2
  , iveHadWorse4
  , lessonLearned2
  , letMeHandleThis
  , liveAndLearn
  , lodgeDebts
  , logicalReasoning
  , lookWhatIFound
  , lookWhatIFound2
  , lucky
  , lucky2
  , lucky3
  , lure1
  , manoAMano1
  , manoAMano2
  , marksmanship1
  , mindOverMatter
  , mindOverMatter2
  , mindWipe1
  , mindWipe3
  , momentOfRespite3
  , monsterSlayer
  , monsterSlayer5
  , moonlightRitual
  , narrowEscape
  , noStoneUnturned
  , noStoneUnturned5
  , occultInvocation
  , oneTwoPunch
  , oneTwoPunch5
  , onTheHunt
  , onTheLam
  , oops
  , payDay1
  , perseverance
  , persuasion
  , premonition
  , preparedForTheWorst
  , preposterousSketches
  , preposterousSketches2
  , quantumFlux
  , recharge2
  , reliable1
  , sacrifice1
  , sceneOfTheCrime
  , scroungeForSupplies
  , searchForTheTruth
  , secondWind
  , seekingAnswers
  , seekingAnswers2
  , shortcut
  , shortcut2
  , sleightOfHand
  , slipAway
  , smuggledGoods
  , snareTrap2
  , sneakAttack
  , sneakAttack2
  , standTogether
  , standTogether3
  , stormOfSpirits
  , sureGamble3
  , taunt
  , taunt2
  , taunt3
  , teamwork
  , thePaintedWorld
  , thinkOnYourFeet
  , timeWarp2
  , trialByFire
  , trueSurvivor3
  , trusted
  , truthFromFiction
  , uncageTheSoul
  , unearthTheAncients
  , unearthTheAncients2
  , unsolvedCase
  , vantagePoint
  , wardOfProtection
  , wardOfProtection2
  , wardOfProtection5
  , waylay
  , willToSurvive
  , willToSurvive3
  , wingingIt
  , workingAHunch
  , youHandleThisOne
  ]

onTheLam :: CardDef
onTheLam = (event "01010" "On the Lam" 1 Neutral)
  { cdCardTraits = setFromList [Tactic]
  , cdSkills = [#intellect, #agility, #wild, #wild]
  , cdFastWindow = Just (TurnBegins Timing.After You)
  }

darkMemory :: CardDef
darkMemory = (event "01013" "Dark Memory" 2 Neutral)
  { cdCardTraits = setFromList [Spell]
  , cdCardSubType = Just Weakness
  , cdCardInHandEffects = True
  }

evidence :: CardDef
evidence = (event "01022" "Evidence!" 1 Guardian)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = setFromList [Insight]
  , cdFastWindow = Just (EnemyDefeated Timing.After You AnyEnemy)
  , cdCriteria = Just
    (Criteria.Criteria
      [ Criteria.LocationExists $ YourLocation <> LocationWithAnyClues
      , Criteria.InvestigatorExists
      $ You
      <> InvestigatorCanDiscoverCluesAt YourLocation
      ]
    )
  , cdAlternateCardCodes = ["01522"]
  }

dodge :: CardDef
dodge = (event "01023" "Dodge" 1 Guardian)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just
    (EnemyAttacks Timing.When (InvestigatorAt YourLocation) AnyEnemyAttack
    $ EnemyWithoutModifier AttacksCannotBeCancelled
    )
  , cdAlternateCardCodes = ["01523", "60113"]
  }

dynamiteBlast :: CardDef
dynamiteBlast = (event "01024" "Dynamite Blast" 5 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Tactic]
  , cdAlternateCardCodes = ["01524"]
  }

extraAmmunition1 :: CardDef
extraAmmunition1 = (event "01026" "Extra Ammunition" 2 Guardian)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Supply]
  , cdLevel = 1
  , cdCriteria = Just
    (Criteria.AssetExists
    $ AssetControlledBy (InvestigatorAt YourLocation)
    <> AssetWithTrait Firearm
    )
  , cdAlternateCardCodes = ["01526"]
  }

mindOverMatter :: CardDef
mindOverMatter = (event "01036" "Mind over Matter" 1 Seeker)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Insight]
  , cdFastWindow = Just (DuringTurn You)
  , cdAlternateCardCodes = ["01536"]
  }

workingAHunch :: CardDef
workingAHunch = (event "01037" "Working a Hunch" 2 Seeker)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = setFromList [Insight]
  , cdFastWindow = Just (DuringTurn You)
  , cdCriteria = Just
    (Criteria.Criteria
      [ Criteria.LocationExists $ YourLocation <> LocationWithAnyClues
      , Criteria.InvestigatorExists
      $ You
      <> InvestigatorCanDiscoverCluesAt YourLocation
      ]
    )
  , cdAlternateCardCodes = ["01537"]
  }

barricade :: CardDef
barricade = (event "01038" "Barricade" 0 Seeker)
  { cdSkills = [#willpower, #intellect, #agility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdAlternateCardCodes = ["01538"]
  }

crypticResearch4 :: CardDef
crypticResearch4 = (event "01043" "Cryptic Research" 0 Seeker)
  { cdCardTraits = setFromList [Insight]
  , cdLevel = 4
  , cdFastWindow = Just (DuringTurn You)
  , cdAlternateCardCodes = ["01543"]
  }

elusive :: CardDef
elusive = (event "01050" "Elusive" 2 Rogue)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Tactic
  , cdFastWindow = Just $ DuringTurn You
  , cdCriteria = Just $ Criteria.AnyCriterion
    [ Criteria.EnemyCriteria $ Criteria.EnemyExists EnemyEngagedWithYou
    , Criteria.LocationExists
    $ RevealedLocation
    <> LocationWithoutEnemies
    <> NotYourLocation
    ]
  , cdAlternateCardCodes = ["01550"]
  }

backstab :: CardDef
backstab = (event "01051" "Backstab" 3 Rogue)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Tactic]
  , cdActions = [Action.Fight]
  , cdAlternateCardCodes = ["01551"]
  }

sneakAttack :: CardDef
sneakAttack = (event "01052" "Sneak Attack" 2 Rogue)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = setFromList [Tactic]
  , cdCriteria = Just
    (Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ EnemyAt YourLocation
    <> ExhaustedEnemy
    )
  , cdAlternateCardCodes = ["01552"]
  }

sureGamble3 :: CardDef
sureGamble3 = (event "01056" "Sure Gamble" 2 Rogue)
  { cdCardTraits = setFromList [Fortune, Insight]
  , cdFastWindow = Just (RevealChaosToken Timing.When You WithNegativeModifier)
  , cdLevel = 3
  , cdAlternateCardCodes = ["01556"]
  }

hotStreak4 :: CardDef
hotStreak4 = (event "01057" "Hot Streak" 3 Rogue)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Fortune]
  , cdLevel = 4
  , cdAlternateCardCodes = ["01557"]
  }

drawnToTheFlame :: CardDef
drawnToTheFlame = (event "01064" "Drawn to the Flame" 0 Mystic)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Insight]
  , cdAlternateCardCodes = ["01564"]
  }

wardOfProtection :: CardDef
wardOfProtection = (event "01065" "Ward of Protection" 1 Mystic)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdFastWindow = Just $ DrawCard
    Timing.When
    You
    (BasicCardMatch NonWeaknessTreachery)
    EncounterDeck
  , cdAlternateCardCodes = ["01565"]
  }

blindingLight :: CardDef
blindingLight = (event "01066" "Blinding Light" 2 Mystic)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Spell]
  , cdActions = [Action.Evade]
  , cdAlternateCardCodes = ["01566"]
  }

mindWipe1 :: CardDef
mindWipe1 = (event "01068" "Mind Wipe" 1 Mystic)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spell]
  , cdLevel = 1
  , cdFastWindow = Just (PhaseBegins Timing.After AnyPhase)
  , cdCriteria = Just
    (Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ EnemyAt YourLocation
    <> NonEliteEnemy
    )
  , cdAlternateCardCodes = ["01568"]
  }

blindingLight2 :: CardDef
blindingLight2 = (event "01069" "Blinding Light" 1 Mystic)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Spell]
  , cdActions = [Action.Evade]
  , cdLevel = 2
  , cdAlternateCardCodes = ["01569"]
  }

cunningDistraction :: CardDef
cunningDistraction = (event "01078" "Cunning Distraction" 5 Survivor)
  { cdSkills = [#willpower, #wild]
  , cdCardTraits = setFromList [Tactic]
  , cdActions = [Action.Evade]
  , cdAlternateCardCodes = ["01578"]
  }

lookWhatIFound :: CardDef
lookWhatIFound = (event "01079" "\"Look what I found!\"" 2 Survivor)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Fortune
  , cdFastWindow =
    Just
    $ SkillTestResult Timing.After You (WhileInvestigating Anywhere)
    $ FailureResult
    $ LessThan
    $ Static 3
  , cdCriteria = Just
    (Criteria.Criteria
      [ Criteria.LocationExists $ YourLocation <> LocationWithAnyClues
      , Criteria.InvestigatorExists
      $ You
      <> InvestigatorCanDiscoverCluesAt YourLocation
      ]
    )
  , cdAlternateCardCodes = ["01579", "60517"]
  }

lucky :: CardDef
lucky = (event "01080" "Lucky!" 1 Survivor)
  { cdCardTraits = setFromList [Fortune]
  , cdFastWindow = Just
    (WouldHaveSkillTestResult Timing.When You AnySkillTest
    $ FailureResult AnyValue
    )
  , cdAlternateCardCodes = ["01580"]
  }

closeCall2 :: CardDef
closeCall2 = (event "01083" "Close Call" 2 Survivor)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Fortune]
  , cdFastWindow = Just
    (EnemyEvaded Timing.After Anyone (EnemyAt YourLocation <> NonWeaknessEnemy))
  , cdLevel = 2
  , cdAlternateCardCodes = ["01583"]
  }

lucky2 :: CardDef
lucky2 = (event "01084" "Lucky!" 1 Survivor)
  { cdCardTraits = setFromList [Fortune]
  , cdFastWindow = Just
    (WouldHaveSkillTestResult Timing.When You AnySkillTest
    $ FailureResult AnyValue
    )
  , cdLevel = 2
  , cdAlternateCardCodes = ["01584"]
  }

willToSurvive3 :: CardDef
willToSurvive3 = (event "01085" "Will to Survive" 4 Survivor)
  { cdSkills = [#combat, #wild]
  , cdCardTraits = setFromList [Spirit]
  , cdFastWindow = Just (DuringTurn You)
  , cdLevel = 3
  , cdAlternateCardCodes = ["01585"]
  }

emergencyCache :: CardDef
emergencyCache = (event "01088" "Emergency Cache" 0 Neutral)
  { cdCardTraits = setFromList [Supply]
  , cdAlternateCardCodes = ["01588"]
  }

searchForTheTruth :: CardDef
searchForTheTruth = (event "02008" "Search for the Truth" 1 Neutral)
  { cdSkills = [#intellect, #intellect, #wild]
  , cdCardTraits = setFromList [Insight]
  }

taunt :: CardDef
taunt = (event "02017" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just (DuringTurn You)
  , cdSkills = [#willpower, #combat]
  }

teamwork :: CardDef
teamwork = (event "02018" "Teamwork" 0 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdSkills = [#wild]
  }

taunt2 :: CardDef
taunt2 = (event "02019" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just (DuringTurn You)
  , cdSkills = [#willpower, #combat, #agility]
  , cdLevel = 2
  }

shortcut :: CardDef
shortcut = (event "02022" "Shortcut" 0 Seeker)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdFastWindow = Just (DuringTurn You)
  , cdCriteria = Just
    (Criteria.LocationExists AccessibleLocation <> Criteria.InvestigatorExists
      (InvestigatorCanMove <> InvestigatorAt YourLocation)
    )
  }

seekingAnswers :: CardDef
seekingAnswers = (event "02023" "Seeking Answers" 1 Seeker)
  { cdSkills = [#intellect, #agility]
  , cdActions = [Action.Investigate]
  , cdCardTraits = singleton Insight
  }

thinkOnYourFeet :: CardDef
thinkOnYourFeet = (event "02025" "Think on Your Feet" 1 Rogue)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Trick
  , cdFastWindow = Just (EnemySpawns Timing.When YourLocation AnyEnemy)
  , cdCriteria = Just
    (Criteria.LocationExists AccessibleLocation
    <> Criteria.InvestigatorExists (You <> InvestigatorCanMove)
    )
  }

bindMonster2 :: CardDef
bindMonster2 = (event "02031" "Bind Monster" 3 Mystic)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = singleton Spell
  , cdActions = [Action.Evade]
  , cdLevel = 2
  }

baitAndSwitch :: CardDef
baitAndSwitch = (event "02034" "Bait and Switch" 1 Survivor)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Trick]
  , cdActions = [Action.Evade]
  }

emergencyAid :: CardDef
emergencyAid = (event "02105" "Emergency Aid" 2 Guardian)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Insight, Science]
  , cdCriteria = Just $ Criteria.AnyCriterion
    [ Criteria.AssetExists
      (HealableAsset ThisCard DamageType $ AssetControlledBy (InvestigatorAt YourLocation)
      <> AllyAsset
      )
    , Criteria.InvestigatorExists
      (HealableInvestigator ThisCard DamageType $ InvestigatorAt YourLocation)
    ]
  }

iveGotAPlan :: CardDef
iveGotAPlan = (event "02107" "\"I've got a plan!\"" 3 Seeker)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdActions = [Action.Fight]
  }

contraband :: CardDef
contraband = (event "02109" "Contraband" 4 Rogue)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Supply, Illicit]
  , cdCriteria = Just $ Criteria.AssetExists
    (AssetControlledBy (InvestigatorAt YourLocation)
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
  { cdSkills = [#combat, #combat]
  , cdCardTraits = singleton Fortune
  , cdFastWindow =
    Just
    $ SkillTestResult Timing.After You (WhileAttackingAnEnemy AnyEnemy)
    $ FailureResult
    $ LessThan
    $ Static 3
  , cdAlternateCardCodes = ["60518"]
  }

flare1 :: CardDef
flare1 = (event "02115" "Flare" 2 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Tactic
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdLevel = 1
  }

standTogether3 :: CardDef
standTogether3 = (event "02148" "Stand Together" 0 Guardian)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Spirit
  , cdCriteria = Just
    $ Criteria.InvestigatorExists (InvestigatorAt YourLocation <> NotYou)
  , cdLevel = 3
  }

imOuttaHere :: CardDef
imOuttaHere = (event "02151" "\"I'm outta here!\"" 0 Rogue)
  { cdSkills = [#agility, #agility]
  , cdCardTraits = setFromList [Trick, Spirit]
  , cdCriteria = Just Criteria.ScenarioCardHasResignAbility
  }

hypnoticGaze :: CardDef
hypnoticGaze = (event "02153" "Hypnotic Gaze" 3 Mystic)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = singleton Spell
  , cdFastWindow = Just
    (EnemyAttacks Timing.When (InvestigatorAt YourLocation) AnyEnemyAttack
    $ EnemyWithoutModifier AttacksCannotBeCancelled
    )
  }

lure1 :: CardDef
lure1 = (event "02156" "Lure" 1 Survivor)
  { cdSkills = [#agility, #agility]
  , cdCardTraits = singleton Trick
  , cdLevel = 1
  }

preparedForTheWorst :: CardDef
preparedForTheWorst = (event "02184" "Prepared for the Worst" 1 Guardian)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = singleton Tactic
  }

preposterousSketches :: CardDef
preposterousSketches = (event "02186" "Preposterous Sketches" 2 Seeker)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = singleton Insight
  , cdCriteria = Just Criteria.ClueOnLocation
  , cdAlternateCardCodes = ["60218"]
  }

emergencyCache2 :: CardDef
emergencyCache2 = (event "02194" "Emergency Cache" 0 Neutral)
  { cdCardTraits = setFromList [Supply]
  , cdLevel = 2
  , cdAlternateCardCodes = ["01693"]
  }

ifItBleeds :: CardDef
ifItBleeds = (event "02225" "\"If it bleeds...\"" 1 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdFastWindow = Just
    (EnemyDefeated Timing.After You $ EnemyWithTrait Monster)
  }

exposeWeakness1 :: CardDef
exposeWeakness1 = (event "02228" "Expose Weakness" 0 Seeker)
  { cdSkills = [#intellect, #combat, #combat]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just FastPlayerWindow
  , cdCriteria = Just
    (Criteria.EnemyCriteria $ Criteria.EnemyExists $ EnemyAt YourLocation)
  , cdLevel = 1
  }

iveHadWorse4 :: CardDef
iveHadWorse4 = (event "02261" "\"I've had worse…\"" 0 Guardian)
  { cdSkills = [#willpower, #willpower, #agility]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just
    (DealtDamageOrHorror Timing.When (SourceIsCancelable AnySource) You)
  , cdLevel = 4
  , cdAlternateCardCodes = ["01684"]
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
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Spell, Insight]
  , cdCriteria = Just Criteria.OwnCardWithDoom
  }

aChanceEncounter :: CardDef
aChanceEncounter = (event "02270" "A Chance Encounter" 1 Survivor)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = singleton Fortune
  , cdCriteria = Just
    $ Criteria.ReturnableCardInDiscard Criteria.AnyPlayerDiscard [Ally]
  }

momentOfRespite3 :: CardDef
momentOfRespite3 = (event "02273" "Moment of Respite" 3 Neutral)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Spirit
  , cdCriteria =
    Just
    $ Criteria.Negate
    $ Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ EnemyAt YourLocation
  , cdLevel = 3
  }

monsterSlayer5 :: CardDef
monsterSlayer5 = (event "02300" "Monster Slayer" 1 Guardian)
  { cdSkills = [#combat, #wild]
  , cdCardTraits = singleton Spirit
  , cdActions = [Action.Fight]
  , cdLevel = 5
  }

decipheredReality5 :: CardDef
decipheredReality5 = (event "02303" "Deciphered Reality" 4 Seeker)
  { cdSkills = [#intellect, #intellect, #willpower]
  , cdCardTraits = singleton Insight
  , cdActions = [Action.Investigate]
  , cdLevel = 5
  }

wardOfProtection5 :: CardDef
wardOfProtection5 = (event "02307" "Ward of Protection" 1 Mystic)
  { cdSkills = [#willpower, #wild]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdFastWindow = Just $ DrawCard
    Timing.When
    You
    (BasicCardMatch NonWeaknessTreachery)
    EncounterDeck
  , cdLevel = 5
  }

thePaintedWorld :: CardDef
thePaintedWorld = (event "03012" "The Painted World" 0 Neutral)
  { cdSkills = [#willpower, #agility, #wild]
  , cdCardTraits = singleton Spell
  , cdFastWindow = Just
    (PlayerHasPlayableCard $ CardIsBeneathInvestigator You <> BasicCardMatch
      (NonExceptional <> EventCard)
    )
  , cdCost = Nothing
  }

buryThemDeep :: CardDef
buryThemDeep = (event "03016" "Bury Them Deep" 0 Neutral)
  { cdSkills = [#willpower, #combat, #wild]
  , cdCardTraits = singleton Task
  , cdFastWindow = Just
    (EnemyDefeated Timing.After Anyone $ NonEliteEnemy <> EnemyAt YourLocation)
  , cdVictoryPoints = Just 1
  }

improvisation :: CardDef
improvisation = (event "03018" "Improvisation" 0 Neutral)
  { cdSkills = [#wild, #wild]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just (DuringTurn You)
  }

letMeHandleThis :: CardDef
letMeHandleThis = (event "03022" "\"Let me handle this!\"" 0 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spirit]
  , cdFastWindow = Just $ DrawCard
    Timing.After
    NotYou
    (BasicCardMatch $ NonPeril <> CardWithOneOf
      (map CardWithType encounterCardTypes)
    )
    EncounterDeck
  }

everVigilant1 :: CardDef
everVigilant1 = (event "03023" "Ever Vigilant" 0 Guardian)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Tactic
  , cdLevel = 1
  , cdCriteria = Just
    (Criteria.PlayableCardExists $ BasicCardMatch AssetCard <> InHandOf You)
  }

noStoneUnturned :: CardDef
noStoneUnturned = (event "03026" "No Stone Unturned" 2 Seeker)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Insight
  , cdCriteria =
    Just
    $ Criteria.InvestigatorExists
    $ InvestigatorAt YourLocation
    <> InvestigatorWithoutModifier CannotManipulateDeck
  }

sleightOfHand :: CardDef
sleightOfHand = (event "03029" "Sleight of Hand" 1 Rogue)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Trick
  , cdFastWindow = Just $ DuringTurn You
  , cdCriteria = Just
    (Criteria.PlayableCardExists $ BasicCardMatch (CardWithTrait Item))
  }

daringManeuver :: CardDef
daringManeuver = (event "03030" "Daring Maneuver" 0 Rogue)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Gambit
  , cdFastWindow =
    Just $ WouldHaveSkillTestResult Timing.When You AnySkillTest $ SuccessResult
      AnyValue
  }

uncageTheSoul :: CardDef
uncageTheSoul = (event "03033" "Uncage the Soul" 0 Mystic)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Spirit
  , cdCriteria = Just
    (Criteria.PlayableCardExistsWithCostReduction 3
    $ InHandOf You
    <> BasicCardMatch
         (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual])
    )
  }

astralTravel :: CardDef
astralTravel = (event "03034" "Astral Travel" 3 Mystic)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = singleton Spell
  , cdActions = [Action.Move]
  , cdCriteria = Just
    (Criteria.LocationExists $ RevealedLocation <> Unblocked <> NotYourLocation)
  }

hidingSpot :: CardDef
hidingSpot = (event "03038" "Hiding Spot" 1 Survivor)
  { cdSkills = [#agility, #agility]
  , cdCardTraits = setFromList [Tactic, Trick]
  , cdFastWindow = Just FastPlayerWindow
  }

heroicRescue :: CardDef
heroicRescue = (event "03106" "Heroic Rescue" 1 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spirit, Tactic]
  , cdFastWindow = Just $ EnemyWouldAttack
    Timing.When
    (NotYou <> InvestigatorAt YourLocation)
    AnyEnemyAttack
    NonEliteEnemy
  }

anatomicalDiagrams :: CardDef
anatomicalDiagrams = (event "03108" "Anatomical Diagrams" 1 Seeker)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just $ DuringTurn Anyone
  , cdCriteria =
    Just
    $ Criteria.InvestigatorExists
        (You <> InvestigatorWithRemainingSanity (AtLeast $ Static 5))
    <> Criteria.EnemyCriteria
         (Criteria.EnemyExists $ EnemyAt YourLocation <> NonEliteEnemy)
  }

ambush1 :: CardDef
ambush1 = (event "03148" "Ambush" 2 Guardian)
  { cdSkills = [#intellect, #combat]
  , cdCardTraits = singleton Tactic
  , cdLevel = 1
  }

forewarned1 :: CardDef
forewarned1 = (event "03150" "Forewarned" 0 Seeker)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Insight
  , cdLevel = 1
  , cdCriteria = Just
    $ Criteria.InvestigatorExists (You <> InvestigatorWithAnyClues)
  , cdFastWindow = Just $ DrawCard
    Timing.When
    You
    (BasicCardMatch NonWeaknessTreachery)
    EncounterDeck
  }

sneakAttack2 :: CardDef
sneakAttack2 = (event "03152" "Sneak Attack" 2 Rogue)
  { cdSkills = [#intellect, #combat, #combat]
  , cdCardTraits = setFromList [Tactic]
  , cdLevel = 2
  , cdCriteria = Just
    (Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ EnemyAt YourLocation
    <> EnemyNotEngagedWithYou
    )
  }

stormOfSpirits :: CardDef
stormOfSpirits = (event "03153" "Storm of Spirits" 3 Mystic)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = singleton Spell
  , cdActions = [Action.Fight]
  }

fightOrFlight :: CardDef
fightOrFlight = (event "03155" "Fight or Flight" 1 Survivor)
  { cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ DuringTurn You
  }

aTestOfWill1 :: CardDef
aTestOfWill1 = (event "03156" "A Test of Will" 1 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ OrWindowMatcher
    [ DrawCard
      Timing.When
      (InvestigatorAt YourLocation)
      (BasicCardMatch $ NonPeril <> NonWeaknessTreachery)
      EncounterDeck
    , DrawCard
      Timing.When
      You
      (BasicCardMatch NonWeaknessTreachery)
      EncounterDeck
    ]
  , cdLevel = 1
  }

devilsLuck :: CardDef
devilsLuck = (event "03157" "Devil's Luck" 1 Survivor)
  { cdSkills = [#agility]
  , cdCardTraits = singleton Fortune
  , cdFastWindow = Just
    (DealtDamageOrHorror Timing.When (SourceIsCancelable AnySource) You)
  , cdLevel = 1
  }

callingInFavors :: CardDef
callingInFavors = (event "03158" "Calling in Favors" 1 Neutral)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Favor
  , cdCriteria = Just (Criteria.AssetExists $ AssetControlledBy You)
  }

illSeeYouInHell :: CardDef
illSeeYouInHell = (event "03189" "\"I'll see you in hell!\"" 0 Guardian)
  { cdSkills = [#combat, #combat]
  , cdCardTraits = singleton Spirit
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  }

logicalReasoning :: CardDef
logicalReasoning = (event "03191" "Logical Reasoning" 2 Seeker)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Insight
  , cdCriteria = Just
    (Criteria.InvestigatorExists (You <> InvestigatorWithAnyClues)
    <> Criteria.AnyCriterion
         [ Criteria.InvestigatorExists
           (HealableInvestigator ThisCard HorrorType $ InvestigatorAt YourLocation)
         , Criteria.TreacheryExists
           (TreacheryWithTrait Terror
           <> TreacheryInThreatAreaOf (InvestigatorAt YourLocation)
           )
         ]
    )
  }

cheapShot :: CardDef
cheapShot = (event "03194" "Cheap Shot" 2 Rogue)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Trick]
  , cdActions = [Action.Fight]
  }

quantumFlux :: CardDef
quantumFlux = (event "03196" "Quantum Flux" 1 Mystic)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Insight
  }

recharge2 :: CardDef
recharge2 = (event "03197" "Recharge" 0 Mystic)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Spell
  , cdCriteria = Just
    (Criteria.AssetExists
    $ AssetControlledBy (InvestigatorAt YourLocation)
    <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Relic]
    )
  , cdLevel = 2
  }

snareTrap2 :: CardDef
snareTrap2 = (event "03199" "Snare Trap" 2 Survivor)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = setFromList [Trap, Improvised]
  , cdCriteria = Just
    (Criteria.Negate
    $ Criteria.AssetExists
    $ AssetIs "03199"
    <> AssetAt YourLocation
    )
  , cdLevel = 2
  }

manoAMano1 :: CardDef
manoAMano1 = (event "03229" "Mano a Mano" 0 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spirit, Bold]
  , cdCriteria = Just $ Criteria.FirstAction <> Criteria.EnemyCriteria
    (Criteria.EnemyExists EnemyEngagedWithYou)
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdLevel = 1
  }

shortcut2 :: CardDef
shortcut2 = (event "03232" "Shortcut" 1 Seeker)
  { cdSkills = [#willpower, #intellect, #agility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdFastWindow = Just $ DuringTurn You
  , cdLevel = 2
  }

waylay :: CardDef
waylay = (event "03237" "Waylay" 3 Survivor)
  { cdSkills = [#agility, #agility]
  , cdCardTraits = singleton Tactic
  , cdCriteria = Just $ Criteria.EnemyCriteria
    (Criteria.EnemyExists
    $ NonEliteEnemy
    <> EnemyAt YourLocation
    <> ExhaustedEnemy
    )
  }

aChanceEncounter2 :: CardDef
aChanceEncounter2 = (event "03238" "A Chance Encounter" 0 Survivor)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = singleton Fortune
  , cdCost = Just DynamicCost
  , cdCriteria = Just
    $ Criteria.ReturnableCardInDiscard Criteria.AnyPlayerDiscard [Ally]
  , cdLevel = 2
  }

emergencyCache3 :: CardDef
emergencyCache3 = (event "03239" "Emergency Cache" 0 Neutral)
  { cdCardTraits = setFromList [Supply]
  , cdLevel = 3
  }

onTheHunt :: CardDef
onTheHunt = (event "03263" "On the Hunt" 1 Guardian)
  { cdCardTraits = singleton Tactic
  , cdFastWindow = Just
    $ WouldDrawEncounterCard Timing.When You (PhaseIs MythosPhase)
  }

guidance :: CardDef
guidance = (event "03265" "Guidance" 0 Seeker)
  { cdCardTraits = singleton Insight
  , cdCriteria = Just $ Criteria.InvestigatorExists
    (NotYou <> InvestigatorAt YourLocation <> YetToTakeTurn)
  }

narrowEscape :: CardDef
narrowEscape = (event "03267" "Narrow Escape" 0 Rogue)
  { cdCardTraits = singleton Fortune
  , cdSkills = [#agility, #agility]
  , cdFastWindow = Just
    (EnemyAttacks
        Timing.When
        (InvestigatorAt YourLocation)
        AttackOfOpportunityAttack
    $ EnemyWithoutModifier AttacksCannotBeCancelled
    )
  }

wardOfProtection2 :: CardDef
wardOfProtection2 = (event "03270" "Ward of Protection" 1 Mystic)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdFastWindow = Just $ DrawCard
    Timing.When
    Anyone
    (BasicCardMatch $ NonPeril <> NonWeaknessTreachery)
    EncounterDeck
  , cdLevel = 2
  }

trueSurvivor3 :: CardDef
trueSurvivor3 = (event "03273" "True Survivor" 3 Survivor)
  { cdCardTraits = singleton Spirit
  , cdCriteria = Just
    $ Criteria.CardInDiscard (Criteria.DiscardOf You) (CardWithTrait Innate)
  , cdLevel = 3
  }

eatLead2 :: CardDef
eatLead2 = (event "03304" "\"Eat Lead!\"" 0 Guardian)
  { cdCardTraits = singleton Tactic
  , cdFastWindow = Just $ ActivateAbility
    Timing.When
    You
    (AssetAbility (AssetWithTrait Firearm) <> AbilityIsAction Action.Fight)
  , cdLevel = 2
  }

eideticMemory3 :: CardDef
eideticMemory3 = (event "03306" "Eidetic Memory" 0 Seeker)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just
    (PlayerHasPlayableCard $ InDiscardOf Anyone <> BasicCardMatch
      (CardWithTrait Insight <> EventCard)
    )
  , cdLevel = 3
  , cdCost = Nothing
  }

noStoneUnturned5 :: CardDef
noStoneUnturned5 = (event "03307" "No Stone Unturned" 2 Seeker)
  { cdCardTraits = singleton Insight
  , cdSkills = [#wild, #intellect]
  , cdFastWindow = Just FastPlayerWindow
  , cdCriteria =
    Just
    $ Criteria.InvestigatorExists
    $ InvestigatorAt YourLocation
    <> InvestigatorWithoutModifier CannotManipulateDeck
  , cdLevel = 5
  }

cheatDeath5 :: CardDef
cheatDeath5 = (event "03310" "Cheat Death" 1 Rogue)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Trick, Fated]
  , cdFastWindow = Just
    $ InvestigatorWouldBeDefeated Timing.When AnySource ByAny You
  , cdLevel = 5
  }

timeWarp2 :: CardDef
timeWarp2 = (event "03311" "Time Warp" 1 Mystic)
  { cdCardTraits = setFromList [Spell, Paradox]
  , cdFastWindow = Just
    $ PerformAction Timing.After (InvestigatorAt YourLocation) AnyAction
  , cdCriteria = Just $ Criteria.ActionCanBeUndone <> Criteria.DuringTurn Anyone
  , cdLevel = 2
  }

infighting3 :: CardDef
infighting3 = (event "03314" "Infighting" 1 Survivor)
  { cdSkills = [#intellect, #intellect, #agility, #agility]
  , cdCardTraits = singleton Trick
  , cdLevel = 3
  , cdFastWindow = Just $ PhaseBegins Timing.After (PhaseIs EnemyPhase)
  }

smuggledGoods :: CardDef
smuggledGoods = (event "04010" "Smuggled Goods" 0 Neutral)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Supply, Illicit]
  , cdCriteria =
    Just
    $ Criteria.Negate
    $ Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ EnemyAt YourLocation
    <> ReadyEnemy
  }

trusted :: CardDef
trusted = (event "04019" "Trusted" 1 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Upgrade
  , cdFastWindow = Just $ DuringTurn You
  , cdCriteria =
    Just $ Criteria.AssetExists $ AssetControlledBy You <> AllyAsset
  }

reliable1 :: CardDef
reliable1 = (event "04020" "Reliable" 1 Guardian)
  { cdSkills = [#intellect]
  , cdCardTraits = singleton Upgrade
  , cdFastWindow = Just $ DuringTurn You
  , cdCriteria =
    Just $ Criteria.AssetExists $ AssetControlledBy You <> AssetWithTrait Item
  , cdLevel = 1
  }

unearthTheAncients :: CardDef
unearthTheAncients = (event "04024" "Unearth the Ancients" 1 Seeker)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Insight
  , cdActions = [Action.Investigate]
  , cdCriteria =
    Just $ Criteria.ExtendedCardExists $ InHandOf You <> BasicCardMatch
      (CardWithClass Seeker <> CardWithType AssetType)
  }

eavesdrop :: CardDef
eavesdrop = (event "04027" "Eavesdrop" 1 Rogue)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Insight, Trick]
  , cdCriteria =
    Just
    $ Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ UnengagedEnemy
    <> EnemyAt YourLocation
  }

youHandleThisOne :: CardDef
youHandleThisOne = (event "04028" "\"You handle this one!\"" 0 Rogue)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Trick
  , cdCriteria = Just (Criteria.InvestigatorExists NotYou)
  , cdFastWindow = Just $ DrawCard
    Timing.When
    You
    (BasicCardMatch $ NonPeril <> IsEncounterCard)
    EncounterDeck
  }

darkProphecy :: CardDef
darkProphecy = (event "04032" "Dark Prophecy" 1 Mystic)
  { cdSkills = [#willpower, #agility]
  , cdCardTraits = singleton Augury
  , cdFastWindow = Just $ WouldRevealChaosToken Timing.When You
  }

improvisedWeapon :: CardDef
improvisedWeapon = (event "04033" "Improvised Weapon" 1 Survivor)
  { cdCardTraits = setFromList [Tactic, Improvised]
  , cdActions = [Action.Fight]
  , cdPlayableFromDiscard = True
  }

dumbLuck :: CardDef
dumbLuck = (event "04034" "Dumb Luck" 2 Survivor)
  { cdSkills = [#agility, #agility]
  , cdCardTraits = singleton Fortune
  , cdFastWindow =
    Just
    $ SkillTestResult Timing.After You (WhileEvadingAnEnemy NonEliteEnemy)
    $ FailureResult
    $ LessThan
    $ Static 3
  , cdAlternateCardCodes = ["60514"]
  }

darkPact :: CardDef
darkPact = (event "04038" "Dark Pact" 2 Neutral)
  { cdCardTraits = singleton Pact
  , cdCardSubType = Just BasicWeakness
  , cdCardInHandEffects = True
  }

sceneOfTheCrime :: CardDef
sceneOfTheCrime = (event "04103" "Scene of the Crime" 2 Guardian)
  { cdSkills = [#combat, #intellect]
  , cdCardTraits = setFromList [Insight, Bold]
  , cdCriteria = Just
    $ Criteria.Criteria [Criteria.FirstAction, Criteria.ClueOnLocation]
  }

marksmanship1 :: CardDef
marksmanship1 = (event "04104" "Marksmanship" 2 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = singleton Tactic
  , cdFastWindow =
    Just
    $ ActivateAbility Timing.When You
    $ AbilityIsAction Action.Fight
    <> AssetAbility (AssetOneOf [AssetWithTrait Firearm, AssetWithTrait Ranged])
  , cdCardInHandEffects = True
  , cdLevel = 1
  }

persuasion :: CardDef
persuasion = (event "04105" "Persuasion" 2 Seeker)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Insight, Trick]
  , cdCriteria =
    Just
    $ Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ NonWeaknessEnemy
    <> EnemyWithTrait Humanoid
    <> EnemyAt YourLocation
  , cdActions = [Action.Parley]
  }

counterspell2 :: CardDef
counterspell2 = (event "04110" "Counterspell" 2 Mystic)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = setFromList [Spell, Blessed]
  , cdFastWindow =
    Just $ RevealChaosToken Timing.When You $ TokenMatchesAny $ map
      TokenFaceIs
      [Token.Skull, Token.Cultist, Token.Tablet, Token.ElderThing]
  , cdLevel = 2
  }

perseverance :: CardDef
perseverance = (event "04111" "Perseverance" 2 Survivor)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ InvestigatorWouldBeDefeated
    Timing.When
    (SourceIsCancelable AnySource)
    (ByAnyOf [ByHorror, ByDamage])
    You
  }

secondWind :: CardDef
secondWind = (event "04149" "Second Wind" 1 Guardian)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Spirit, Bold]
  , cdCriteria = Just $ Criteria.FirstAction <> Criteria.InvestigatorExists (HealableInvestigator ThisCard DamageType You)
  }

truthFromFiction :: CardDef
truthFromFiction = (event "04152" "Truth from Fiction" 2 Seeker)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Insight
  , cdCriteria = Just $ Criteria.ClueOnLocation <> Criteria.AssetExists
    (AssetControlledBy You <> AssetWithUseType Uses.Secret)
  }

customAmmunition3 :: CardDef
customAmmunition3 = (event "04193" "Custom Ammunition" 3 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Upgrade, Supply, Blessed]
  , cdCriteria = Just $ Criteria.AssetExists
    (AssetControlledBy (InvestigatorAt YourLocation)
    <> AssetWithTrait Firearm
    <> NotAsset
         (AssetWithAttachedEvent $ EventCardMatch $ cardIs customAmmunition3)
    )
  , cdFastWindow = Just $ DuringTurn You
  }

exposeWeakness3 :: CardDef
exposeWeakness3 = (event "04195" "Expose Weakness" 0 Seeker)
  { cdSkills = [#intellect, #combat, #wild]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just FastPlayerWindow
  , cdCriteria = Just
    (Criteria.EnemyCriteria $ Criteria.EnemyExists $ EnemyAt YourLocation)
  , cdLevel = 3
  }

premonition :: CardDef
premonition = (event "04199" "Premonition" 0 Mystic)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Augury
  , cdFastWindow = Just FastPlayerWindow
  }

liveAndLearn :: CardDef
liveAndLearn = (event "04200" "Live and Learn" 0 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ SkillTestEnded Timing.After You SkillTestWasFailed
  , cdAlternateCardCodes = ["60516"]
  }

againstAllOdds2 :: CardDef
againstAllOdds2 = (event "04202" "Against All Odds" 2 Survivor)
  { cdCardTraits = singleton Spirit
  , cdSkills = [#willpower, #combat, #agility]
  , cdFastWindow = Just
    $ InitiatedSkillTest Timing.When You AnySkillType GreaterThanBaseValue
  , cdLevel = 2
  }

slipAway :: CardDef
slipAway = (event "04232" "Against All Odds" 2 Rogue)
  { cdCardTraits = singleton Trick
  , cdSkills = [#intellect, #agility]
  , cdActions = [Action.Evade]
  }

payDay1 :: CardDef
payDay1 = (event "04233" "Pay Day" 0 Rogue)
  { cdCardTraits = setFromList [Illicit, Fated]
  , cdLevel = 1
  }

sacrifice1 :: CardDef
sacrifice1 = (event "04234" "Sacrifice" 0 Mystic)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Ritual
  , cdCriteria =
    Just
    $ Criteria.AssetExists
    $ AssetWithClass Mystic
    <> AssetControlledBy You
    <> DiscardableAsset
  , cdLevel = 1
  }

bloodEclipse3 :: CardDef
bloodEclipse3 = (event "04266" "Blood Eclipse" 1 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdActions = [Action.Fight]
  , cdAdditionalCost = Just $ UpTo 3 $ InvestigatorDamageCost
    ThisCard
    You
    DamageAny
    1
  , cdLevel = 3
  }

coupDeGrace :: CardDef
coupDeGrace = (event "04269" "Coup de Grâce" 2 Rogue)
  { cdSkills = [#combat, #combat]
  , cdCardTraits = setFromList [Tactic, Fated]
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdCriteria = Just $ Criteria.EnemyCriteria $ Criteria.EnemyExists $ EnemyAt
    YourLocation <> EnemyCanBeDamagedBySource ThisCard
  }

wingingIt :: CardDef
wingingIt = (event "04272" "Winging It" 1 Survivor)
  { cdCardTraits = setFromList [Tactic, Improvised]
  , cdActions = [Action.Investigate]
  , cdPlayableFromDiscard = True
  }

vantagePoint :: CardDef
vantagePoint = (event "04306" "Vantage Point" 1 Seeker)
  { cdCardTraits = singleton Insight
  , cdSkills = [#intellect, #agility]
  , cdCriteria = Just $ Criteria.DuringTurn Anyone
  , cdFastWindow = Just $ OrWindowMatcher
    [ PutLocationIntoPlay Timing.After Anyone Anywhere
    , RevealLocation Timing.After Anyone Anywhere
    ]
  }

impromptuBarrier :: CardDef
impromptuBarrier = (event "04312" "Impromptu Barrier" 1 Survivor)
  { cdCardTraits = setFromList [Tactic, Improvised]
  , cdActions = [Action.Evade]
  , cdPlayableFromDiscard = True
  }

alterFate3 :: CardDef
alterFate3 = (event "04313" "Alter Fate" 1 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Spell, Blessed]
  , cdFastWindow = Just FastPlayerWindow
  , cdCriteria = Just $ Criteria.TreacheryExists $ NotTreachery (TreacheryOnEnemy EliteEnemy) <> TreacheryIsNonWeakness
  , cdLevel = 3
  }

unsolvedCase :: CardDef
unsolvedCase = (event "05010" "Unsolved Case" 4 Neutral)
  { cdCardTraits = setFromList [Insight, Mystery]
  , cdCardSubType = Just Weakness
  , cdCardInHandEffects = True
  }

lodgeDebts :: CardDef
lodgeDebts = (event "05012" "\"Lodge\" Debts" 10 Neutral)
  { cdCardTraits = singleton Pact
  , cdCardSubType = Just Weakness
  , cdCardInHandEffects = True
  }

darkInsight :: CardDef
darkInsight = (event "05014" "Dark Insight" 2 Neutral)
  { cdCardTraits = singleton Insight
  , cdFastWindow = Just $ OrWindowMatcher
    [ DrawCard
      Timing.When
      (InvestigatorAt YourLocation)
      (BasicCardMatch $ NonPeril <> CardWithOneOf [IsEncounterCard, WeaknessCard])
      AnyDeck
    , DrawCard
      Timing.When
      You
      (BasicCardMatch $ CardWithOneOf [IsEncounterCard, WeaknessCard])
      AnyDeck
    ]
  }

imDoneRunnin :: CardDef
imDoneRunnin = (event "05016" "\"I'm done runnin'!\"" 0 Neutral)
  { cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ DuringTurn You
  }

delayTheInevitable :: CardDef
delayTheInevitable = (event "05021" "Delay the Inevitable" 2 Guardian)
  { cdSkills = [#combat, #agility]
  , cdCardTraits = setFromList [Insight, Spirit, Tactic]
  , cdFastWindow = Just $ DuringTurn You
  }

denyExistence :: CardDef
denyExistence = (event "05032" "Deny Existence" 0 Mystic)
  { cdSkills = [#wild]
  , cdCardTraits = setFromList [Spell, Paradox]
  , cdFastWindow = Nothing -- TODO: big window
  }

trialByFire :: CardDef
trialByFire = (event "05281" "Trial By Fire" 3 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ DuringTurn You
  }

bloodRite :: CardDef
bloodRite = (event "05317" "Blood-Rite" 0 Seeker)
  { cdSkills = [#willpower, #intellect, #combat]
  , cdCardTraits = singleton Spell
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  }

astoundingRevelation :: CardDef
astoundingRevelation = (event "06023" "Astounding Revelation" 0 Seeker)
  { cdSkills = [#intellect]
  , cdCardTraits = setFromList [Research]
  , cdCost = Nothing
  , cdCardInSearchEffects = True
  }

firstWatch :: CardDef
firstWatch = (event "06110" "First Watch" 1 Guardian)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
  }

scroungeForSupplies :: CardDef
scroungeForSupplies = (event "06165" "Scrounge for Supplies" 0 Survivor)
  { cdCardTraits = singleton Fortune
  , cdCriteria = Just
    $ Criteria.CardInDiscard (Criteria.DiscardOf You) (CardWithLevel 0)
  }

unearthTheAncients2 :: CardDef
unearthTheAncients2 = (event "08039" "Unearth the Ancients" 0 Seeker)
  { cdSkills = [#intellect, #intellect, #agility]
  , cdCardTraits = singleton Insight
  , cdActions = [Action.Investigate]
  , cdCriteria =
    Just $ Criteria.ExtendedCardExists $ InHandOf You <> BasicCardMatch
      (CardWithClass Seeker <> CardWithType AssetType)
  , cdLevel = 2
  }

dynamiteBlast2 :: CardDef
dynamiteBlast2 = (event "50002" "Dynamite Blast" 4 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Tactic]
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdLevel = 2
  }

barricade3 :: CardDef
barricade3 = (event "50004" "Barricade" 0 Seeker)
  { cdSkills = [#willpower, #intellect, #agility]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdLevel = 3
  }

hotStreak2 :: CardDef
hotStreak2 = (event "50006" "Hot Streak" 5 Rogue)
  { cdSkills = [#willpower]
  , cdCardTraits = setFromList [Fortune]
  , cdLevel = 2
  }

mindWipe3 :: CardDef
mindWipe3 = (event "50008" "Mind Wipe" 1 Mystic)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spell]
  , cdLevel = 3
  , cdFastWindow = Just $ PhaseBegins Timing.After AnyPhase
  }

preposterousSketches2 :: CardDef
preposterousSketches2 = (event "51003" "Preposterous Sketches" 0 Seeker)
  { cdSkills = [#willpower, #intellect]
  , cdCardTraits = singleton Insight
  , cdCriteria = Just Criteria.ClueOnLocation
  , cdLevel = 2
  }

contraband2 :: CardDef
contraband2 = (event "51005" "Contraband" 3 Rogue)
  { cdSkills = [#willpower, #intellect, #intellect]
  , cdCardTraits = setFromList [Supply, Illicit]
  , cdLevel = 2
  , cdCriteria = Just $ Criteria.AssetExists
    (AssetControlledBy (InvestigatorAt YourLocation)
    <> AssetOneOf [AssetWithUseType Uses.Ammo, AssetWithUseType Uses.Supply]
    )
  }

bloodEclipse1 :: CardDef
bloodEclipse1 = (event "53001" "Blood Eclipse" 1 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spell, Spirit]
  , cdActions = [Action.Fight]
  , cdAdditionalCost = Just $ InvestigatorDamageCost ThisCard You DamageAny 2
  , cdLevel = 1
  }

cleanThemOut :: CardDef
cleanThemOut = (event "60111" "Clean Them Out" 0 Guardian)
  { cdCardTraits = setFromList [Spirit, Tactic]
  , cdActions = [Action.Fight]
  , cdSkills = [#willpower, #combat]
  }

counterpunch :: CardDef
counterpunch = (event "60112" "Counterpunch" 0 Guardian)
  { cdCardTraits = setFromList [Spirit, Tactic]
  , cdActions = [Action.Fight]
  , cdSkills = [#combat, #agility]
  , cdFastWindow = Just
    $ EnemyAttacksEvenIfCancelled Timing.After You AnyEnemyAttack AnyEnemy
  }

-- We need to override the action check for this card because of multiple actions,
-- but even if we can not fight or engage the enemy, if we can move it this should
-- still be playable
getOverHere :: CardDef
getOverHere = (event "60114" "\"Get over here!\"" 2 Guardian)
  { cdCardTraits = setFromList [Spirit, Tactic]
  , cdActions = [Action.Engage, Action.Fight]
  , cdSkills = [#willpower, #combat]
  , cdCriteria =
    Just
    $ Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ NonEliteEnemy
    <> EnemyOneOf
         [ EnemyAt YourLocation <> EnemyOneOf [CanEngageEnemy, CanFightEnemy]
         , EnemyAt $ ConnectedFrom YourLocation
         ]
  , cdOverrideActionPlayableIfCriteriaMet = True
  }

glory :: CardDef
glory = (event "60115" "Glory" 1 Guardian)
  { cdCardTraits = singleton Spirit
  , cdSkills = [#intellect, #intellect]
  , cdFastWindow = Just $ EnemyDefeated Timing.After You AnyEnemy
  }

monsterSlayer :: CardDef
monsterSlayer = (event "60116" "Monster Slayer" 0 Guardian)
  { cdCardTraits = singleton Spirit
  , cdActions = [Action.Fight]
  , cdSkills = [#wild]
  }

oneTwoPunch :: CardDef
oneTwoPunch = (event "60117" "One-Two Punch" 2 Guardian)
  { cdCardTraits = setFromList [Spirit, Tactic]
  , cdActions = [Action.Fight]
  , cdSkills = [#combat]
  }

standTogether :: CardDef
standTogether = (event "60118" "Stand Together" 0 Guardian)
  { cdCardTraits = singleton Spirit
  , cdSkills = [#willpower]
  , cdCriteria = Just
    (Criteria.InvestigatorExists $ NotYou <> InvestigatorAt YourLocation)
  }

evidence1 :: CardDef
evidence1 = (event "60120" "Evidence!" 1 Guardian)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just (EnemyDefeated Timing.After You AnyEnemy)
  , cdCriteria = Just
    (Criteria.Criteria
      [ Criteria.LocationExists $ YourLocation <> LocationWithAnyClues
      , Criteria.InvestigatorExists
      $ You
      <> InvestigatorCanDiscoverCluesAt YourLocation
      ]
    )
  , cdLevel = 1
  }

galvanize1 :: CardDef
galvanize1 = (event "60121" "Galvanize" 2 Guardian)
  { cdSkills = [#willpower, #willpower]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ DuringTurn You
  , cdLevel = 1
  }

counterpunch2 :: CardDef
counterpunch2 = (event "60122" "Counterpunch" 0 Guardian)
  { cdSkills = [#combat, #combat, #agility]
  , cdCardTraits = setFromList [Spirit, Tactic]
  , cdFastWindow = Just $ EnemyAttacks Timing.When You AnyEnemyAttack AnyEnemy
  , cdLevel = 2
  }

-- We need to override the action check for this card because of multiple actions,
-- but even if we can not fight or engage the enemy, if we can move it this should
-- still be playable
getOverHere2 :: CardDef
getOverHere2 = (event "60123" "\"Get over here!\"" 2 Guardian)
  { cdCardTraits = setFromList [Spirit, Tactic]
  , cdActions = [Action.Engage, Action.Fight]
  , cdSkills = [#willpower, #willpower, #combat]
  , cdFastWindow = Just FastPlayerWindow
  , cdCriteria =
    Just
    $ Criteria.EnemyCriteria
    $ Criteria.EnemyExists
    $ NonEliteEnemy
    <> EnemyOneOf
         [ EnemyAt YourLocation <> EnemyOneOf [CanEngageEnemy, CanFightEnemy]
         , EnemyAt $ ConnectedFrom YourLocation
         , EnemyAt $ LocationWithDistanceFrom 2 YourLocation
         ]
  , cdOverrideActionPlayableIfCriteriaMet = True
  , cdLevel = 2
  }

lessonLearned2 :: CardDef
lessonLearned2 = (event "60124" "Lesson Learned" 1 Guardian)
  { cdCardTraits = setFromList [Insight, Spirit]
  , cdSkills = [#willpower, #intellect, #intellect]
  , cdFastWindow = Just
    $ DealtDamage Timing.After (SourceIsEnemyAttack AnyEnemy) You
  , cdCriteria = Just $ Criteria.Criteria
    [ Criteria.LocationExists $ YourLocation <> LocationWithAnyClues
    , Criteria.InvestigatorExists
    $ You
    <> InvestigatorCanDiscoverCluesAt YourLocation
    ]
  , cdLevel = 2
  }

manoAMano2 :: CardDef
manoAMano2 = (event "60125" "Mano a Mano" 0 Guardian)
  { cdSkills = [#willpower, #combat]
  , cdCardTraits = setFromList [Spirit, Bold]
  , cdCriteria = Just $ Criteria.FirstAction <> Criteria.EnemyCriteria
    (Criteria.EnemyExists EnemyEngagedWithYou)
  , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , cdLevel = 2
  }

dynamiteBlast3 :: CardDef
dynamiteBlast3 = (event "60129" "Dynamite Blast" 4 Guardian)
  { cdSkills = [#willpower, #willpower, #combat, #combat]
  , cdCardTraits = setFromList [Tactic]
  , cdLevel = 3
  }

taunt3 :: CardDef
taunt3 = (event "60130" "Taunt" 1 Guardian)
  { cdCardTraits = setFromList [Tactic]
  , cdFastWindow = Just $ DuringTurn You
  , cdSkills = [#willpower, #willpower, #combat, #agility]
  , cdLevel = 3
  }

oneTwoPunch5 :: CardDef
oneTwoPunch5 = (event "60132" "One-Two Punch" 2 Guardian)
  { cdCardTraits = setFromList [Spirit, Tactic]
  , cdActions = [Action.Fight]
  , cdSkills = [#combat, #combat, #combat, #combat]
  , cdLevel = 5
  }

burningTheMidnightOil :: CardDef
burningTheMidnightOil = (event "60214" "Burning the Midnight Oil" 0 Seeker)
  { cdSkills = [#intellect, #agility]
  , cdCardTraits = singleton Insight
  , cdActions = [Action.Investigate]
  }

crypticWritings :: CardDef
crypticWritings = (event "60215" "Cryptic Writings" 0 Seeker)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Insight
  , cdCardInHandEffects = True
  }

extensiveResearch :: CardDef
extensiveResearch = (event "60216" "Extensive Research" 12 Seeker)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Insight
  , cdCardInHandEffects = True
  , cdCriteria = Just $ Criteria.Criteria
    [ Criteria.LocationExists $ YourLocation <> LocationWithAnyClues
    , Criteria.InvestigatorExists
    $ You
    <> InvestigatorCanDiscoverCluesAt YourLocation
    ]
  }

occultInvocation :: CardDef
occultInvocation = (event "60217" "Occult Invocation" 2 Seeker)
  { cdSkills = [#combat, #intellect]
  , cdCardTraits = singleton Spell
  , cdAdditionalCost = Just $ UpTo 2 $ HandDiscardCost 1 AnyCard
  , cdActions = [Action.Fight]
  }

glimpseTheUnthinkable1 :: CardDef
glimpseTheUnthinkable1 = (event "60221" "Glimpse the Unthinkable" 0 Seeker)
  { cdSkills = [#intellect, #intellect]
  , cdCardTraits = singleton Insight
  , cdLevel = 1
  , cdCriteria = Just
    $ Criteria.AnyCriterion [Criteria.CanDrawCards, Criteria.CanManipulateDeck]
  }

crypticWritings2 :: CardDef
crypticWritings2 = (event "60224" "Cryptic Writings" 0 Seeker)
  { cdSkills = [#intellect, #wild]
  , cdCardTraits = singleton Insight
  , cdCardInHandEffects = True
  , cdLevel = 2
  }

iveGotAPlan2 :: CardDef
iveGotAPlan2 = (event "60225" "\"I've got a plan!\"" 2 Seeker)
  { cdSkills = [#intellect, #intellect, #combat]
  , cdCardTraits = setFromList [Insight, Tactic]
  , cdLevel = 2
  , cdActions = [Action.Fight]
  }

mindOverMatter2 :: CardDef
mindOverMatter2 = (event "60226" "Mind over Matter" 1 Seeker)
  { cdSkills = [#combat, #agility, #wild]
  , cdCardTraits = singleton Insight
  , cdFastWindow = Just (DuringTurn You)
  , cdLevel = 2
  }

seekingAnswers2 :: CardDef
seekingAnswers2 = (event "60227" "Seeking Answers" 1 Seeker)
  { cdSkills = [#intellect, #agility, #agility]
  , cdActions = [Action.Investigate]
  , cdCardTraits = singleton Insight
  , cdLevel = 2
  }

willToSurvive :: CardDef
willToSurvive = (event "60512" "Will to Survive" 4 Survivor)
  { cdSkills = [#combat, #wild]
  , cdCardTraits = setFromList [Spirit]
  , cdFastWindow = Just (DuringTurn You)
  }

aTestOfWill :: CardDef
aTestOfWill = (event "60513" "A Test of Will" 1 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ OrWindowMatcher
    [ DrawCard
      Timing.When
      (InvestigatorAt YourLocation)
      (BasicCardMatch $ NonPeril <> NonWeaknessTreachery)
      EncounterDeck
    , DrawCard
      Timing.When
      You
      (BasicCardMatch NonWeaknessTreachery)
      EncounterDeck
    ]
  }

gritYourTeeth :: CardDef
gritYourTeeth = (event "60515" "Grit Your Teeth" 1 Survivor)
  { cdSkills = [#wild]
  , cdCardTraits = singleton Spirit
  , cdFastWindow =
    Just $ SkillTestResult Timing.After You AnySkillTest $ FailureResult
      AnyValue
  }

aTestOfWill2 :: CardDef
aTestOfWill2 = (event "60523" "A Test of Will" 0 Survivor)
  { cdSkills = [#willpower]
  , cdCardTraits = singleton Spirit
  , cdFastWindow = Just $ OrWindowMatcher
    [ DrawCard
      Timing.When
      (InvestigatorAt YourLocation)
      (BasicCardMatch $ NonPeril <> NonWeaknessTreachery)
      EncounterDeck
    , DrawCard
      Timing.When
      You
      (BasicCardMatch NonWeaknessTreachery)
      EncounterDeck
    ]
  , cdLevel = 2
  }

lookWhatIFound2 :: CardDef
lookWhatIFound2 = (event "60524" "\"Look what I found!\"" 2 Survivor)
  { cdSkills = [#intellect, #intellect, #agility]
  , cdCardTraits = singleton Fortune
  , cdLevel = 2
  , cdCriteria = Just
    (Criteria.Criteria
      [ Criteria.LocationExists
      $ LocationMatchAny [YourLocation, ConnectedLocation]
      <> LocationWithAnyClues
      , Criteria.InvestigatorExists $ You <> InvestigatorCanDiscoverCluesAt
        (LocationMatchAny [YourLocation, ConnectedLocation])
      ]
    )
  , cdFastWindow =
    Just
    $ SkillTestResult Timing.After You (WhileInvestigating Anywhere)
    $ FailureResult
    $ LessThan
    $ Static 4
  }

dumbLuck2 :: CardDef
dumbLuck2 = (event "60525" "Dumb Luck" 2 Survivor)
  { cdSkills = [#willpower, #agility, #agility]
  , cdCardTraits = singleton Fortune
  , cdFastWindow =
    Just
    $ SkillTestResult Timing.After You (WhileEvadingAnEnemy NonEliteEnemy)
    $ FailureResult
    $ LessThan
    $ Static 4
  , cdLevel = 2
  }

lucky3 :: CardDef
lucky3 = (event "60528" "Lucky!" 0 Survivor)
  { cdCardTraits = singleton Fortune
  , cdFastWindow = Just
    (WouldHaveSkillTestResult
        Timing.When
        (InvestigatorAt YourLocation)
        AnySkillTest
    $ FailureResult AnyValue
    )
  , cdLevel = 3
  }
