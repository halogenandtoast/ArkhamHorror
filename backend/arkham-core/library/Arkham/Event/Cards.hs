module Arkham.Event.Cards where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Agenda.AdvancementReason
import Arkham.Asset.Uses qualified as Uses
import Arkham.Capability
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Card.Cost
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.Criteria (Criterion, exists, notExists, youExist)
import Arkham.Criteria qualified as Criteria
import Arkham.Damage
import Arkham.GameValue
import Arkham.History.Types
import Arkham.Id
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier (ModifierType (..))
import Arkham.Name
import Arkham.SkillType ()
import Arkham.SlotType
import Arkham.Source
import Arkham.Strategy
import Arkham.Trait

event :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
event cardCode name cost = baseEvent cardCode name cost . singleton

baseEvent :: CardCode -> Name -> Int -> Set ClassSymbol -> CardDef
baseEvent cardCode name cost classSymbols =
  (emptyCardDef cardCode name EventType)
    { cdCost = Just (StaticCost cost)
    , cdClassSymbols = classSymbols
    }

multiClassEvent :: CardCode -> Name -> Int -> [ClassSymbol] -> CardDef
multiClassEvent cCode name cost classSymbols = baseEvent cCode name cost (setFromList classSymbols)

signature :: InvestigatorId -> CardDef -> CardDef
signature iid cd = cd {cdDeckRestrictions = [Signature iid], cdLevel = Nothing}

allPlayerEventCards :: Map CardCode CardDef
allPlayerEventCards =
  mapFromList
    $ concatMap
      toCardCodePairs
      [ aChanceEncounter
      , aChanceEncounter2
      , aGlimmerOfHope
      , aTestOfWill
      , aTestOfWill1
      , aTestOfWill2
      , aWatchfulPeace3
      , aceInTheHole3
      , actOfDesperation
      , againstAllOdds2
      , ambush1
      , anatomicalDiagrams
      , astoundingRevelation
      , astralTravel
      , alterFate1
      , alterFate3
      , backstab
      , backstab3
      , banish1
      , baitAndSwitch
      , baitAndSwitch3
      , barricade
      , barricade3
      , bellyOfTheBeast
      , bindMonster2
      , blindingLight
      , blindingLight2
      , bloodEclipse1
      , bloodEclipse3
      , bloodRite
      , breakingAndEntering
      , breakingAndEntering2
      , burningTheMidnightOil
      , buryThemDeep
      , butterflyEffect1
      , callingInFavors
      , cheapShot
      , cheapShot2
      , cheatDeath5
      , cleanThemOut
      , closeCall2
      , connectTheDots
      , contraband
      , contraband2
      , coupDeGrace
      , counterpunch
      , counterpunch2
      , counterspell2
      , crackTheCase
      , crypticResearch4
      , crypticWritings
      , crypticWritings2
      , cunningDistraction
      , customAmmunition3
      , daringManeuver
      , daringManeuver2
      , darkInsight
      , darkMemory
      , darkPact
      , darkProphecy
      , decipheredReality5
      , decoy
      , deepKnowledge
      , delayTheInevitable
      , delveTooDeep
      , denyExistence
      , denyExistence5
      , devilsLuck
      , dodge
      , dodge2
      , drawnToTheFlame
      , dumbLuck
      , dumbLuck2
      , dynamiteBlast
      , dynamiteBlast2
      , dynamiteBlast3
      , easyMark1
      , eatLead
      , eatLead2
      , eavesdrop
      , eideticMemory3
      , eldritchInspiration
      , eldritchInspiration1
      , elusive
      , emergencyAid
      , emergencyCache
      , emergencyCache2
      , emergencyCache3
      , enchantWeapon3
      , etherealForm
      , etherealSlip
      , etherealSlip2
      , eucatastrophe3
      , everVigilant1
      , evidence
      , evidence1
      , exposeWeakness1
      , exposeWeakness3
      , extensiveResearch
      , extensiveResearch1
      , extraAmmunition1
      , faustianBargain
      , fightOrFlight
      , firstWatch
      , flare1
      , followed
      , foolMeOnce1
      , forewarned1
      , fortuitousDiscovery
      , fortuneOrFate2
      , galvanize1
      , gazeOfOuraxsh2
      , getBehindMe
      , getOverHere
      , getOverHere2
      , ghastlyRevelation
      , glimpseTheUnthinkable1
      , glimpseTheUnthinkable5
      , glory
      , gritYourTeeth
      , guidance
      , hallow3
      , handOfFate
      , harmonyRestored2
      , heroicRescue
      , heroicRescue2
      , hidingSpot
      , hotStreak2
      , hotStreak4
      , hypnoticGaze
      , hypnoticGaze2
      , ifItBleeds
      , illSeeYouInHell
      , imDoneRunnin
      , imOuttaHere
      , impromptuBarrier
      , improvisation
      , improvisedWeapon
      , inTheShadows
      , infighting3
      , intelReport
      , interrogate
      , iveGotAPlan
      , iveGotAPlan2
      , iveHadWorse2
      , iveHadWorse4
      , keepFaith
      , keepFaith2
      , knowledgeIsPower
      , lessonLearned2
      , letGodSortThemOut
      , letMeHandleThis
      , liveAndLearn
      , lodgeDebts
      , logicalReasoning
      , logicalReasoning4
      , lookWhatIFound
      , lookWhatIFound2
      , lucidDreaming2
      , lucky
      , lucky2
      , lucky3
      , lure1
      , lure2
      , manipulateDestiny2
      , manoAMano1
      , manoAMano2
      , marksmanship1
      , meditativeTrance
      , mindOverMatter
      , mindOverMatter2
      , mindWipe1
      , mindWipe3
      , momentOfRespite3
      , moneyTalks
      , moneyTalks2
      , monsterSlayer
      , monsterSlayer5
      , moonlightRitual
      , mysteriesRemain
      , mystifyingSong
      , narrowEscape
      , noStoneUnturned
      , noStoneUnturned5
      , nothingLeftToLose3
      , obscureStudies
      , occultEvidence
      , occultInvocation
      , oneTwoPunch
      , oneTwoPunch5
      , onTheHunt
      , onTheLam
      , oops
      , oops2
      , openGate
      , parallelFates
      , parallelFates2
      , payDay1
      , perseverance
      , persuasion
      , pilfer
      , pilfer3
      , practiceMakesPerfect
      , premonition
      , preparedForTheWorst
      , preposterousSketches
      , preposterousSketches2
      , quantumFlux
      , radiantSmite1
      , readTheSigns
      , recharge2
      , recharge4
      , reliable1
      , riastrad1
      , righteousHunt1
      , riteOfEquilibrium5
      , sacrifice1
      , sceneOfTheCrime
      , scoutAhead
      , scroungeForSupplies
      , searchForTheTruth
      , secondWind
      , seekingAnswers
      , seekingAnswers2
      , shortcut
      , shortcut2
      , shrineOfTheMoirai3
      , sleightOfHand
      , slipAway
      , slipAway2
      , smallFavor
      , smuggledGoods
      , snareTrap2
      , sneakAttack
      , sneakAttack2
      , sneakBy
      , soothingMelody
      , spectralRazor
      , standTogether
      , standTogether3
      , stargazing1
      , stirringUpTrouble1
      , stormOfSpirits
      , stormOfSpirits3
      , sureGamble3
      , sweepingKick1
      , swiftReflexes
      , swiftReload2
      , taunt
      , taunt2
      , taunt3
      , teamwork
      , telescopicSight3
      , temptFate
      , thePaintedWorld
      , theStygianEye3
      , theTruthBeckons
      , thinkOnYourFeet
      , thinkOnYourFeet2
      , thirdTimesACharm2
      , tidesOfFate
      , timeWarp2
      , toeToToe
      , trialByFire
      , trialByFire3
      , trueSurvivor3
      , trusted
      , truthFromFiction
      , truthFromFiction2
      , uncageTheSoul
      , underSurveillance1
      , unearthTheAncients
      , unearthTheAncients2
      , unsolvedCase
      , vantagePoint
      , voiceOfRa
      , wardOfProtection
      , wardOfProtection2
      , wardOfProtection5
      , wardOfRadiance
      , warningShot
      , waylay
      , wellMaintained1
      , willToSurvive
      , willToSurvive3
      , wingingIt
      , wordOfCommand2
      , workingAHunch
      , youHandleThisOne
      , youOweMeOne
      ]

allEncounterEventCards :: Map CardCode CardDef
allEncounterEventCards = mapFromList $ concatMap toCardCodePairs [theStarsAreRight]

canDiscoverCluesAtYourLocation :: Criterion
canDiscoverCluesAtYourLocation =
  Criteria.Criteria
    [ exists $ YourLocation <> LocationWithAnyClues
    , exists $ You <> InvestigatorCanDiscoverCluesAt YourLocation
    ]

onTheLam :: CardDef
onTheLam =
  signature "01003"
    $ (event "01010" "On the Lam" 1 Neutral)
      { cdCardTraits = setFromList [Tactic]
      , cdSkills = [#intellect, #agility, #wild, #wild]
      , cdFastWindow = Just (TurnBegins #after You)
      , cdAlternateCardCodes = ["01510"]
      }

darkMemory :: CardDef
darkMemory =
  (event "01013" "Dark Memory" 2 Neutral)
    { cdCardTraits = setFromList [Spell]
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdAlternateCardCodes = ["01513"]
    }

evidence :: CardDef
evidence =
  (event "01022" "Evidence!" 1 Guardian)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ EnemyDefeated #after You ByAny AnyEnemy
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdAlternateCardCodes = ["01522"]
    }

dodge :: CardDef
dodge =
  (event "01023" "Dodge" 1 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdAlternateCardCodes = ["01523", "60113"]
    }

dynamiteBlast :: CardDef
dynamiteBlast =
  (event "01024" "Dynamite Blast" 5 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Tactic]
    , cdAlternateCardCodes = ["01524"]
    , cdCriteria = Just Criteria.CanDealDamage
    }

extraAmmunition1 :: CardDef
extraAmmunition1 =
  (event "01026" "Extra Ammunition" 2 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Supply]
    , cdLevel = Just 1
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetWithTrait Firearm
    , cdAlternateCardCodes = ["01526"]
    }

mindOverMatter :: CardDef
mindOverMatter =
  (event "01036" "Mind over Matter" 1 Seeker)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdAlternateCardCodes = ["01536"]
    }

workingAHunch :: CardDef
workingAHunch =
  (event "01037" "Working a Hunch" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdAlternateCardCodes = ["01537"]
    }

barricade :: CardDef
barricade =
  (event "01038" "Barricade" 0 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdAlternateCardCodes = ["01538"]
    }

crypticResearch4 :: CardDef
crypticResearch4 =
  (event "01043" "Cryptic Research" 0 Seeker)
    { cdCardTraits = setFromList [Insight]
    , cdLevel = Just 4
    , cdFastWindow = Just $ DuringTurn You
    , cdAlternateCardCodes = ["01543"]
    , cdCriteria =
        Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> can.draw.cards FromPlayerCardEffect
    }

elusive :: CardDef
elusive =
  (event "01050" "Elusive" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Tactic
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists EnemyEngagedWithYou
            , Criteria.CanMoveTo $ RevealedLocation <> LocationWithoutEnemies
            ]
    , cdAlternateCardCodes = ["01550"]
    }

backstab :: CardDef
backstab =
  (event "01051" "Backstab" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#fight]
    , cdAlternateCardCodes = ["01551"]
    }

sneakAttack :: CardDef
sneakAttack =
  (event "01052" "Sneak Attack" 2 Rogue)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdCriteria =
        Just $ exists (EnemyAt YourLocation <> ExhaustedEnemy) <> Criteria.CanDealDamage
    , cdAlternateCardCodes = ["01552"]
    }

sureGamble3 :: CardDef
sureGamble3 =
  (event "01056" "Sure Gamble" 2 Rogue)
    { cdCardTraits = setFromList [Fortune, Insight]
    , cdFastWindow = Just $ RevealChaosToken #when You WithNegativeModifier
    , cdLevel = Just 3
    , cdAlternateCardCodes = ["01556"]
    }

hotStreak4 :: CardDef
hotStreak4 =
  (event "01057" "Hot Streak" 3 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune]
    , cdLevel = Just 4
    , cdAlternateCardCodes = ["01557"]
    }

drawnToTheFlame :: CardDef
drawnToTheFlame =
  (event "01064" "Drawn to the Flame" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight]
    , cdAlternateCardCodes = ["01564"]
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

wardOfProtection :: CardDef
wardOfProtection =
  (event "01065" "Ward of Protection" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just $ DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
    , cdAlternateCardCodes = ["01565"]
    }

blindingLight :: CardDef
blindingLight =
  (event "01066" "Blinding Light" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell]
    , cdActions = [#evade]
    , cdAlternateCardCodes = ["01566"]
    }

mindWipe1 :: CardDef
mindWipe1 =
  (event "01068" "Mind Wipe" 1 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = Just 1
    , cdFastWindow = Just $ PhaseBegins #after AnyPhase
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> NonEliteEnemy
    , cdAlternateCardCodes = ["01568"]
    }

blindingLight2 :: CardDef
blindingLight2 =
  (event "01069" "Blinding Light" 1 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell]
    , cdActions = [#evade]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01569"]
    }

cunningDistraction :: CardDef
cunningDistraction =
  (event "01078" "Cunning Distraction" 5 Survivor)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#evade]
    , cdAlternateCardCodes = ["01578"]
    }

lookWhatIFound :: CardDef
lookWhatIFound =
  (event "01079" "\"Look what I found!\"" 2 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Fortune
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileInvestigating Anywhere) $ FailureResult $ lessThan 3
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdAlternateCardCodes = ["01579", "60517"]
    }

lucky :: CardDef
lucky =
  (event "01080" "Lucky!" 1 Survivor)
    { cdCardTraits = setFromList [Fortune]
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ FailureResult AnyValue
    , cdAlternateCardCodes = ["01580"]
    }

closeCall2 :: CardDef
closeCall2 =
  (event "01083" "Close Call" 2 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Fortune]
    , cdFastWindow =
        Just $ EnemyEvaded #after Anyone (EnemyAt YourLocation <> NonWeaknessEnemy <> NonEliteEnemy)
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01583"]
    }

lucky2 :: CardDef
lucky2 =
  (event "01084" "Lucky!" 1 Survivor)
    { cdCardTraits = setFromList [Fortune]
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ FailureResult AnyValue
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01584"]
    }

willToSurvive3 :: CardDef
willToSurvive3 =
  (event "01085" "Will to Survive" 4 Survivor)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    , cdAlternateCardCodes = ["01585"]
    }

emergencyCache :: CardDef
emergencyCache =
  (event "01088" "Emergency Cache" 0 Neutral)
    { cdCardTraits = setFromList [Supply]
    , cdAlternateCardCodes = ["01588"]
    , cdCriteria = Just $ exists $ You <> can.gain.resources
    }

searchForTheTruth :: CardDef
searchForTheTruth =
  signature "02002"
    $ (event "02008" "Search for the Truth" 1 Neutral)
      { cdSkills = [#intellect, #intellect, #wild]
      , cdCardTraits = setFromList [Insight]
      , cdCriteria = Just $ exists $ You <> can.draw.cards FromPlayerCardEffect
      }

taunt :: CardDef
taunt =
  (event "02017" "Taunt" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdSkills = [#willpower, #combat]
    }

teamwork :: CardDef
teamwork =
  (event "02018" "Teamwork" 0 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdSkills = [#wild]
    , cdCriteria =
        Just $ exists $ affectsOthers $ NotInvestigator You <> InvestigatorAt YourLocation
    }

taunt2 :: CardDef
taunt2 =
  (event "02019" "Taunt" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdSkills = [#willpower, #combat, #agility]
    , cdLevel = Just 2
    }

shortcut :: CardDef
shortcut =
  (event "02022" "Shortcut" 0 Seeker)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ exists
          $ affectsOthers
          $ can.move
          <> InvestigatorAt YourLocation
          <> InvestigatorCanMoveTo ThisCard AccessibleLocation
    }

seekingAnswers :: CardDef
seekingAnswers =
  (event "02023" "Seeking Answers" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdActions = [#investigate]
    , cdCardTraits = singleton Insight
    }

thinkOnYourFeet :: CardDef
thinkOnYourFeet =
  (event "02025" "Think on Your Feet" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdFastWindow = Just $ EnemySpawns #when YourLocation AnyEnemy
    , cdCriteria = Just $ exists AccessibleLocation <> exists (You <> can.move)
    }

bindMonster2 :: CardDef
bindMonster2 =
  (event "02031" "Bind Monster" 3 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Spell
    , cdActions = [#evade]
    , cdLevel = Just 2
    }

baitAndSwitch :: CardDef
baitAndSwitch =
  (event "02034" "Bait and Switch" 1 Survivor)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#evade]
    }

emergencyAid :: CardDef
emergencyAid =
  (event "02105" "Emergency Aid" 2 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Science]
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists
                $ HealableAsset ThisCard DamageType
                $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                <> #ally
            , exists
                $ affectsOthers
                $ HealableInvestigator ThisCard DamageType
                $ InvestigatorAt YourLocation
            ]
    }

iveGotAPlan :: CardDef
iveGotAPlan =
  (event "02107" "\"I've got a plan!\"" 3 Seeker)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdActions = [#fight]
    }

contraband :: CardDef
contraband =
  (event "02109" "Contraband" 4 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Supply, Illicit]
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> oneOf [AssetWithUses Uses.Ammo, AssetWithUses Uses.Supply]
          <> AssetNotAtUseLimit
    }

delveTooDeep :: CardDef
delveTooDeep =
  (event "02111" "Delve Too Deep" 1 Mystic)
    { cdCardTraits = setFromList [Insight]
    , cdVictoryPoints = Just 1
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

-- TODO: Oops might not be playable if 0 damage would be dealt, we might want
-- to capture that
oops :: CardDef
oops =
  (event "02113" "Oops!" 2 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = singleton Fortune
    , cdCriteria =
        Just $ exists (EnemyAt YourLocation <> NotEnemy AttackedEnemy) <> Criteria.CanDealDamage
    , cdFastWindow =
        Just
          $ SkillTestResult #after You (WhileAttackingAnEnemy EnemyEngagedWithYou)
          $ FailureResult
          $ lessThan 3
    , cdAlternateCardCodes = ["60518"]
    }

flare1 :: CardDef
flare1 =
  (event "02115" "Flare" 2 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Tactic
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 1
    , cdCriteria = Just $ exists $ affectsOthers can.manipulate.deck
    }

standTogether3 :: CardDef
standTogether3 =
  (event "02148" "Stand Together" 0 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ exists (affectsOthers $ InvestigatorAt YourLocation <> NotYou)
          <> exists
            ( affectsOthers
                $ InvestigatorAt YourLocation
                <> oneOf [can.gain.resources, can.draw.cards FromPlayerCardEffect]
            )
    , cdLevel = Just 3
    }

imOuttaHere :: CardDef
imOuttaHere =
  (event "02151" "\"I'm outta here!\"" 0 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Trick, Spirit]
    , cdCriteria = Just Criteria.ScenarioCardHasResignAbility
    }

hypnoticGaze :: CardDef
hypnoticGaze =
  (event "02153" "Hypnotic Gaze" 3 Mystic)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Spell
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdAlternateCardCodes = ["60414"]
    }

lure1 :: CardDef
lure1 =
  (event "02156" "Lure" 1 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Trick
    , cdLevel = Just 1
    }

preparedForTheWorst :: CardDef
preparedForTheWorst =
  (event "02184" "Prepared for the Worst" 1 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Tactic
    }

preposterousSketches :: CardDef
preposterousSketches =
  (event "02186" "Preposterous Sketches" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria = Just Criteria.ClueOnLocation
    , cdAlternateCardCodes = ["60218"]
    }

emergencyCache2 :: CardDef
emergencyCache2 =
  (event "02194" "Emergency Cache" 0 Neutral)
    { cdCardTraits = setFromList [Supply]
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01693"]
    , cdCriteria = Just $ exists $ You <> can.gain.resources
    }

ifItBleeds :: CardDef
ifItBleeds =
  (event "02225" "\"If it bleeds...\"" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdFastWindow = Just $ EnemyDefeated #after You ByAny $ EnemyWithTrait Monster
    }

exposeWeakness1 :: CardDef
exposeWeakness1 =
  (event "02228" "Expose Weakness" 0 Seeker)
    { cdSkills = [#intellect, #combat, #combat]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyWithFight
    , cdLevel = Just 1
    }

iveHadWorse4 :: CardDef
iveHadWorse4 =
  (event "02261" "\"I've had worse…\"" 0 Guardian)
    { cdSkills = [#willpower, #willpower, #agility]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) You
    , cdLevel = Just 4
    , cdAlternateCardCodes = ["01684"]
    }

aceInTheHole3 :: CardDef
aceInTheHole3 =
  (event "02266" "Ace in the Hole" 0 Rogue)
    { cdCardTraits = singleton Trick
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    , cdExceptional = True
    }

moonlightRitual :: CardDef
moonlightRitual =
  (event "02267" "Moonlight Ritual" 0 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Spell, Insight]
    , cdCriteria = Just Criteria.OwnCardWithDoom
    }

aChanceEncounter :: CardDef
aChanceEncounter =
  (event "02270" "A Chance Encounter" 1 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Fortune
    , cdCriteria = Just $ Criteria.ReturnableCardInDiscard Criteria.AnyPlayerDiscard [Ally]
    }

momentOfRespite3 :: CardDef
momentOfRespite3 =
  (event "02273" "Moment of Respite" 3 Neutral)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdCriteria = Just $ Criteria.Negate $ exists $ EnemyAt YourLocation
    , cdLevel = Just 3
    }

monsterSlayer5 :: CardDef
monsterSlayer5 =
  (event "02300" "Monster Slayer" 1 Guardian)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = singleton Spirit
    , cdActions = [#fight]
    , cdLevel = Just 5
    }

decipheredReality5 :: CardDef
decipheredReality5 =
  (event "02303" "Deciphered Reality" 4 Seeker)
    { cdSkills = [#intellect, #intellect, #willpower]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    , cdLevel = Just 5
    }

wardOfProtection5 :: CardDef
wardOfProtection5 =
  (event "02307" "Ward of Protection" 1 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just
          $ DrawCard
            #when
            You
            (CanCancelAllEffects $ BasicCardMatch IsEncounterCard)
            EncounterDeck
    , cdLevel = Just 5
    }

thePaintedWorld :: CardDef
thePaintedWorld =
  signature "03003"
    $ (event "03012" "The Painted World" 0 Neutral)
      { cdSkills = [#willpower, #agility, #wild]
      , cdCardTraits = singleton Spell
      , cdSkipPlayWindows = True
      , cdFastWindow =
          Just
            $ PlayerHasPlayableCard
            $ CardIsBeneathInvestigator You
            <> basic (NonExceptional <> #event)
      , cdCost = Nothing
      }

buryThemDeep :: CardDef
buryThemDeep =
  signature "03005"
    $ (event "03016" "Bury Them Deep" 0 Neutral)
      { cdSkills = [#willpower, #combat, #wild]
      , cdCardTraits = singleton Task
      , cdFastWindow = Just $ EnemyDefeated #after Anyone ByAny $ NonEliteEnemy <> EnemyAt YourLocation
      , cdVictoryPoints = Just 1
      }

improvisation :: CardDef
improvisation =
  signature "03006"
    $ (event "03018" "Improvisation" 0 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = singleton Insight
      , cdFastWindow = Just $ DuringTurn You
      }

letMeHandleThis :: CardDef
letMeHandleThis =
  (event "03022" "\"Let me handle this!\"" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow =
        Just $ DrawCard #after (affectsOthers NotYou) (basic $ NonPeril <> IsEncounterCard) AnyDeck
    }

everVigilant1 :: CardDef
everVigilant1 =
  (event "03023" "Ever Vigilant" 0 Guardian)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Tactic
    , cdLevel = Just 1
    , cdCriteria = Just $ Criteria.PlayableCardExistsWithCostReduction 1 $ #asset <> InHandOf You
    }

noStoneUnturned :: CardDef
noStoneUnturned =
  (event "03026" "No Stone Unturned" 2 Seeker)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> can.manipulate.deck
    }

sleightOfHand :: CardDef
sleightOfHand =
  (event "03029" "Sleight of Hand" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ Criteria.PlayableCardExists PaidCost $ InHandOf You <> #item
    }

daringManeuver :: CardDef
daringManeuver =
  (event "03030" "Daring Maneuver" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Gambit
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ SuccessResult AnyValue
    , cdAlternateCardCodes = ["60313"]
    }

uncageTheSoul :: CardDef
uncageTheSoul =
  (event "03033" "Uncage the Soul" 0 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ Criteria.PlayableCardExistsWithCostReduction 3
          $ InHandOf You
          <> basic (oneOf [CardWithTrait Spell, CardWithTrait Ritual])
    }

astralTravel :: CardDef
astralTravel =
  (event "03034" "Astral Travel" 3 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdActions = [#move]
    , cdCriteria = Just $ exists $ RevealedLocation <> Unblocked <> NotYourLocation
    , cdAlternateCardCodes = ["60413"]
    }

hidingSpot :: CardDef
hidingSpot =
  (event "03038" "Hiding Spot" 1 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just FastPlayerWindow
    }

heroicRescue :: CardDef
heroicRescue =
  (event "03106" "Heroic Rescue" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow =
        Just
          $ EnemyWouldAttack
            #when
            (affectsOthers $ NotYou <> InvestigatorAt YourLocation)
            AnyEnemyAttack
            NonEliteEnemy
    }

anatomicalDiagrams :: CardDef
anatomicalDiagrams =
  (event "03108" "Anatomical Diagrams" 1 Seeker)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn Anyone
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorWithRemainingSanity (atLeast 5))
          <> exists (EnemyAt YourLocation <> NonEliteEnemy)
    }

ambush1 :: CardDef
ambush1 =
  (event "03148" "Ambush" 2 Guardian)
    { cdSkills = [#intellect, #combat]
    , cdCardTraits = singleton Tactic
    , cdLevel = Just 1
    }

forewarned1 :: CardDef
forewarned1 =
  (event "03150" "Forewarned" 0 Seeker)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 1
    , cdCriteria = Just $ exists (You <> InvestigatorWithAnyClues)
    , cdFastWindow =
        Just $ DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
    }

sneakAttack2 :: CardDef
sneakAttack2 =
  (event "03152" "Sneak Attack" 2 Rogue)
    { cdSkills = [#intellect, #combat, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdLevel = Just 2
    , cdCriteria =
        Just $ exists (EnemyAt YourLocation <> EnemyNotEngagedWithYou) <> Criteria.CanDealDamage
    }

stormOfSpirits :: CardDef
stormOfSpirits =
  (event "03153" "Storm of Spirits" 3 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    }

fightOrFlight :: CardDef
fightOrFlight =
  (event "03155" "Fight or Flight" 1 Survivor)
    { cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    }

aTestOfWill1 :: CardDef
aTestOfWill1 =
  (event "03156" "A Test of Will" 1 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ DrawCard
                #when
                (InvestigatorAt YourLocation)
                (CanCancelRevelationEffect $ basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard
                #when
                You
                (CanCancelRevelationEffect $ basic NonWeaknessTreachery)
                EncounterDeck
            ]
    , cdLevel = Just 1
    }

devilsLuck :: CardDef
devilsLuck =
  (event "03157" "Devil's Luck" 1 Survivor)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Fortune
    , cdFastWindow = Just $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) You
    , cdLevel = Just 1
    }

callingInFavors :: CardDef
callingInFavors =
  (event "03158" "Calling in Favors" 1 Neutral)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Favor
    , cdCriteria = Just $ exists $ AssetControlledBy You
    }

illSeeYouInHell :: CardDef
illSeeYouInHell =
  (event "03189" "\"I'll see you in hell!\"" 0 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = singleton Spirit
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

logicalReasoning :: CardDef
logicalReasoning =
  (event "03191" "Logical Reasoning" 2 Seeker)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorWithAnyClues)
          <> Criteria.AnyCriterion
            [ exists
                $ HealableInvestigator ThisCard HorrorType
                $ InvestigatorAt YourLocation
            , exists
                $ TreacheryWithTrait Terror
                <> TreacheryInThreatAreaOf (affectsOthers $ InvestigatorAt YourLocation)
            ]
    }

cheapShot :: CardDef
cheapShot =
  (event "03194" "Cheap Shot" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#fight]
    , cdAlternateCardCodes = ["60312"]
    }

quantumFlux :: CardDef
quantumFlux =
  (event "03196" "Quantum Flux" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Insight
    }

recharge2 :: CardDef
recharge2 =
  (event "03197" "Recharge" 0 Mystic)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> oneOf [AssetWithTrait Spell, AssetWithTrait Relic]
    , cdLevel = Just 2
    }

snareTrap2 :: CardDef
snareTrap2 =
  (event "03199" "Snare Trap" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Trap, Improvised]
    , cdCriteria = Just $ Criteria.Negate $ exists $ "Snare Trap" <> AssetAt YourLocation
    , cdLevel = Just 2
    }

manoAMano1 :: CardDef
manoAMano1 =
  (event "03229" "Mano a Mano" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just $ Criteria.FirstAction <> exists EnemyEngagedWithYou <> Criteria.CanDealDamage
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 1
    }

shortcut2 :: CardDef
shortcut2 =
  (event "03232" "Shortcut" 1 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 2
    }

waylay :: CardDef
waylay =
  (event "03237" "Waylay" 3 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Tactic
    , cdCriteria =
        Just $ exists $ NonEliteEnemy <> EnemyAt YourLocation <> ExhaustedEnemy <> EnemyWithEvade
    }

aChanceEncounter2 :: CardDef
aChanceEncounter2 =
  (event "03238" "A Chance Encounter" 0 Survivor)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Fortune
    , cdCost = Just DynamicCost
    , cdCriteria = Just $ Criteria.ReturnableCardInDiscard Criteria.AnyPlayerDiscard [Ally]
    , cdLevel = Just 2
    }

emergencyCache3 :: CardDef
emergencyCache3 =
  (event "03239" "Emergency Cache" 0 Neutral)
    { cdCardTraits = setFromList [Supply]
    , cdLevel = Just 3
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists $ You <> can.gain.resources
            , exists
                $ AssetWithUseType Uses.Supply
                <> AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                <> AssetNotAtUseLimit
            ]
    }

onTheHunt :: CardDef
onTheHunt =
  (event "03263" "On the Hunt" 1 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow = Just $ WouldDrawEncounterCard #when You #mythos
    , cdSkills = [#intellect, #combat]
    }

guidance :: CardDef
guidance =
  (event "03265" "Guidance" 0 Seeker)
    { cdCardTraits = singleton Insight
    , cdCriteria =
        Just $ exists $ affectsOthers $ NotYou <> InvestigatorAt YourLocation <> YetToTakeTurn
    , cdSkills = [#wild]
    }

narrowEscape :: CardDef
narrowEscape =
  (event "03267" "Narrow Escape" 0 Rogue)
    { cdCardTraits = singleton Fortune
    , cdSkills = [#agility, #agility]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            You
            (CancelableEnemyAttack AttackOfOpportunityAttack)
            AnyEnemy
    }

wardOfProtection2 :: CardDef
wardOfProtection2 =
  (event "03270" "Ward of Protection" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just
          $ DrawCard
            #when
            (affectsOthers Anyone)
            (CanCancelRevelationEffect $ basic $ NonPeril <> NonWeaknessTreachery)
            EncounterDeck
    , cdLevel = Just 2
    }

trueSurvivor3 :: CardDef
trueSurvivor3 =
  (event "03273" "True Survivor" 3 Survivor)
    { cdCardTraits = singleton Spirit
    , cdCriteria = Just $ Criteria.CardInDiscard (Criteria.DiscardOf You) (CardWithTrait Innate)
    , cdLevel = Just 3
    }

eatLead2 :: CardDef
eatLead2 =
  (event "03304" "\"Eat lead!\"" 0 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow =
        Just
          $ ActivateAbility
            #when
            You
            (AssetAbility (AssetWithTrait Firearm <> AssetWithUses Uses.Ammo) <> AbilityIsAction #fight)
    , cdLevel = Just 2
    , cdSkills = [#combat, #agility]
    }

eideticMemory3 :: CardDef
eideticMemory3 =
  (event "03306" "Eidetic Memory" 0 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just $ PlayerHasPlayableCard $ InDiscardOf Anyone <> basic (CardWithTrait Insight <> #event)
    , cdLevel = Just 3
    , cdCost = Nothing
    }

noStoneUnturned5 :: CardDef
noStoneUnturned5 =
  (event "03307" "No Stone Unturned" 2 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#wild, #intellect]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just $ exists $ affectsOthers $ InvestigatorAt YourLocation <> can.manipulate.deck
    , cdLevel = Just 5
    }

cheatDeath5 :: CardDef
cheatDeath5 =
  (event "03310" "Cheat Death" 1 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Trick, Fated]
    , cdFastWindow = Just $ InvestigatorWouldBeDefeated #when ByAny You
    , cdLevel = Just 5
    }

timeWarp2 :: CardDef
timeWarp2 =
  (event "03311" "Time Warp" 1 Mystic)
    { cdCardTraits = setFromList [Spell, Paradox]
    , cdFastWindow =
        Just $ PerformAction #after (affectsOthers $ InvestigatorAt YourLocation) AnyAction
    , cdCriteria = Just $ Criteria.ActionCanBeUndone <> Criteria.DuringTurn Anyone
    , cdLevel = Just 2
    }

infighting3 :: CardDef
infighting3 =
  (event "03314" "Infighting" 1 Survivor)
    { cdSkills = [#intellect, #intellect, #agility, #agility]
    , cdCardTraits = singleton Trick
    , cdLevel = Just 3
    , cdFastWindow = Just $ PhaseBegins #after #enemy
    }

smuggledGoods :: CardDef
smuggledGoods =
  signature "04003"
    $ (event "04010" "Smuggled Goods" 0 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Supply, Illicit]
      , cdCriteria = Just $ Criteria.Negate $ exists $ EnemyAt YourLocation <> ReadyEnemy
      }

trusted :: CardDef
trusted =
  (event "04019" "Trusted" 1 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Upgrade
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #ally
    }

reliable1 :: CardDef
reliable1 =
  (event "04020" "Reliable" 1 Guardian)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Upgrade
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists $ AssetControlledBy You <> #item
    , cdLevel = Just 1
    }

unearthTheAncients :: CardDef
unearthTheAncients =
  (event "04024" "Unearth the Ancients" 1 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    , cdCriteria = Just $ Criteria.ExtendedCardExists $ InHandOf You <> basic (#seeker <> #asset)
    }

eavesdrop :: CardDef
eavesdrop =
  (event "04027" "Eavesdrop" 1 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdCriteria = Just $ exists $ UnengagedEnemy <> EnemyAt YourLocation <> EnemyWithEvade
    }

youHandleThisOne :: CardDef
youHandleThisOne =
  (event "04028" "\"You handle this one!\"" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdCriteria = Just (exists $ affectsOthers NotYou)
    , cdFastWindow = Just $ DrawCard #when You (basic $ NonPeril <> IsEncounterCard) EncounterDeck
    }

darkProphecy :: CardDef
darkProphecy =
  (event "04032" "Dark Prophecy" 1 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Augury
    , cdFastWindow = Just $ WouldRevealChaosToken #when You
    , cdAlternateCardCodes = ["60417"]
    }

improvisedWeapon :: CardDef
improvisedWeapon =
  (event "04033" "Improvised Weapon" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#fight]
    , cdPlayableFromDiscard = True
    }

dumbLuck :: CardDef
dumbLuck =
  (event "04034" "Dumb Luck" 2 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Fortune
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdFastWindow =
        Just
          $ SkillTestResult #after You (WhileEvadingAnEnemy NonEliteEnemy)
          $ FailureResult
          $ lessThan 3
    , cdAlternateCardCodes = ["60514"]
    }

darkPact :: CardDef
darkPact =
  (event "04038" "Dark Pact" 2 Neutral)
    { cdCardTraits = singleton Pact
    , cdCardSubType = Just BasicWeakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    , cdDeckRestrictions = [CampaignModeOnly]
    }

sceneOfTheCrime :: CardDef
sceneOfTheCrime =
  (event "04103" "Scene of the Crime" 2 Guardian)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = setFromList [Insight, Bold]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdCriteria = Just $ Criteria.Criteria [Criteria.FirstAction, Criteria.ClueOnLocation]
    }

marksmanship1 :: CardDef
marksmanship1 =
  (event "04104" "Marksmanship" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Tactic
    , cdFastWindow =
        Just
          $ ActivateAbility #when You
          $ AbilityIsAction #fight
          <> AssetAbility (oneOf [AssetWithTrait Firearm, AssetWithTrait Ranged])
    , cdCardInHandEffects = True
    , cdLevel = Just 1
    }

persuasion :: CardDef
persuasion =
  (event "04105" "Persuasion" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdCriteria =
        Just
          $ exists (NonWeaknessEnemy <> EnemyWithTrait Humanoid <> EnemyAt YourLocation)
          <> exists (You <> can.target.encounterDeck)
    , cdActions = [Action.Parley]
    }

counterspell2 :: CardDef
counterspell2 =
  (event "04110" "Counterspell" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow =
        Just
          $ RevealChaosToken #when You
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [#skull, #cultist, #tablet, #elderthing]
    , cdLevel = Just 2
    }

perseverance :: CardDef
perseverance =
  (event "04111" "Perseverance" 2 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ InvestigatorWouldBeDefeated
            #when
            (BySource (SourceIsCancelable AnySource) <> ByAnyOf [ByHorror, ByDamage])
            You
    }

secondWind :: CardDef
secondWind =
  (event "04149" "Second Wind" 1 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just $ Criteria.FirstAction <> exists (HealableInvestigator ThisCard DamageType You)
    }

truthFromFiction :: CardDef
truthFromFiction =
  (event "04152" "Truth from Fiction" 2 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria =
        Just
          $ Criteria.ClueOnLocation
          <> exists (AssetControlledBy You <> AssetCanHaveUses Uses.Secret)
    }

customAmmunition3 :: CardDef
customAmmunition3 =
  (event "04193" "Custom Ammunition" 3 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Upgrade, Supply, Blessed]
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetWithTrait Firearm
          <> NotAsset (AssetWithAttachedEvent $ EventCardMatch $ cardIs customAmmunition3)
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    }

exposeWeakness3 :: CardDef
exposeWeakness3 =
  (event "04195" "Expose Weakness" 0 Seeker)
    { cdSkills = [#intellect, #combat, #wild]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just $ exists $ EnemyAt YourLocation <> EnemyWithFight
    , cdLevel = Just 3
    }

premonition :: CardDef
premonition =
  (event "04199" "Premonition" 0 Mystic)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Augury
    , cdFastWindow = Just FastPlayerWindow
    }

liveAndLearn :: CardDef
liveAndLearn =
  (event "04200" "Live and Learn" 0 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ SkillTestEnded #after You SkillTestWasFailed
    , cdAlternateCardCodes = ["60516"]
    }

againstAllOdds2 :: CardDef
againstAllOdds2 =
  (event "04202" "Against All Odds" 2 Survivor)
    { cdCardTraits = singleton Spirit
    , cdSkills = [#willpower, #combat, #agility]
    , cdFastWindow = Just $ InitiatedSkillTest #when You AnySkillType GreaterThanBaseValue #any
    , cdLevel = Just 2
    }

slipAway :: CardDef
slipAway =
  (event "04232" "Slip Away" 2 Rogue)
    { cdCardTraits = singleton Trick
    , cdSkills = [#intellect, #agility]
    , cdActions = [#evade]
    , cdAlternateCardCodes = ["60314"]
    }

payDay1 :: CardDef
payDay1 =
  (event "04233" "Pay Day" 0 Rogue)
    { cdCardTraits = setFromList [Illicit, Fated]
    , cdLevel = Just 1
    }

sacrifice1 :: CardDef
sacrifice1 =
  (event "04234" "Sacrifice" 0 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Ritual
    , cdCriteria = Just $ exists $ #mystic <> AssetControlledBy You <> DiscardableAsset
    , cdLevel = Just 1
    }

bloodEclipse3 :: CardDef
bloodEclipse3 =
  (event "04266" "Blood Eclipse" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdActions = [#fight]
    , cdAdditionalCost = Just $ UpTo 3 $ InvestigatorDamageCost ThisCard You DamageAny 1
    , cdLevel = Just 3
    }

coupDeGrace :: CardDef
coupDeGrace =
  (event "04269" "Coup de Grâce" 2 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Tactic, Fated]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdCriteria =
        Just
          $ exists (EnemyAt YourLocation <> EnemyCanBeDamagedBySource ThisCard)
          <> Criteria.CanDealDamage
    }

wingingIt :: CardDef
wingingIt =
  (event "04272" "Winging It" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#investigate]
    , cdPlayableFromDiscard = True
    }

vantagePoint :: CardDef
vantagePoint =
  (event "04306" "Vantage Point" 1 Seeker)
    { cdCardTraits = singleton Insight
    , cdSkills = [#intellect, #agility]
    , cdCriteria = Just $ Criteria.DuringTurn Anyone
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ PutLocationIntoPlay #after Anyone Anywhere
            , RevealLocation #after Anyone Anywhere
            ]
    }

impromptuBarrier :: CardDef
impromptuBarrier =
  (event "04312" "Impromptu Barrier" 1 Survivor)
    { cdCardTraits = setFromList [Tactic, Improvised]
    , cdActions = [#evade]
    , cdPlayableFromDiscard = True
    }

alterFate3 :: CardDef
alterFate3 =
  (event "04313" "Alter Fate" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just
          $ exists
          $ NotTreachery (TreacheryOnEnemy EliteEnemy)
          <> TreacheryIsNonWeakness
    , cdLevel = Just 3
    }

unsolvedCase :: CardDef
unsolvedCase =
  (event "05010" "Unsolved Case" 4 Neutral)
    { cdCardTraits = setFromList [Insight, Mystery]
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    }

lodgeDebts :: CardDef
lodgeDebts =
  (event "05012" "Lodge \"Debts\"" 10 Neutral)
    { cdCardTraits = singleton Pact
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdCardInHandEffects = True
    }

darkInsight :: CardDef
darkInsight =
  signature "05004"
    $ (event "05014" "Dark Insight" 2 Neutral)
      { cdCardTraits = singleton Insight
      , cdFastWindow =
          Just
            $ OrWindowMatcher
              [ DrawCard
                  #when
                  (affectsOthers $ InvestigatorAt YourLocation)
                  (basic $ NonPeril <> oneOf [IsEncounterCard, WeaknessCard])
                  AnyDeck
              , DrawCard #when You (basic $ oneOf [IsEncounterCard, WeaknessCard]) AnyDeck
              ]
      }

imDoneRunnin :: CardDef
imDoneRunnin =
  signature "05005"
    $ (event "05016" "\"I'm done runnin'!\"" 0 Neutral)
      { cdSkills = [#combat, #agility, #wild]
      , cdCardTraits = singleton Spirit
      , cdFastWindow = Just $ DuringTurn You
      }

mystifyingSong :: CardDef
mystifyingSong =
  signature "05006"
    $ (event "05018" "Mystifying Song" 3 Neutral)
      { cdSkills = [#wild, #wild]
      , cdCardTraits = setFromList [Spell, Song]
      , cdFastWindow = Just $ AgendaWouldAdvance #when DoomThreshold AnyAgenda
      , cdAlternateCardCodes = ["99002"]
      }

interrogate :: CardDef
interrogate =
  (event "05020" "Interrogate" 2 Guardian)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = setFromList [Tactic, Insight]
    , cdCriteria = Just $ exists $ EnemyWithTrait Humanoid <> EnemyAt YourLocation
    , cdActions = [Action.Parley]
    }

delayTheInevitable :: CardDef
delayTheInevitable =
  (event "05021" "Delay the Inevitable" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Insight, Spirit, Tactic]
    , cdFastWindow = Just $ DuringTurn You
    }

connectTheDots :: CardDef
connectTheDots =
  (event "05025" "Connect the Dots" 4 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DiscoveringLastClue #after You YourLocation
    , cdCriteria =
        Just
          $ exists
          $ LocationWithLowerPrintedShroudThan YourLocation
          <> LocationWithDiscoverableCluesBy You
    }

moneyTalks :: CardDef
moneyTalks =
  (event "05029" "Money Talks" 0 Rogue)
    { cdCardTraits = setFromList [Favor, Gambit]
    , cdFastWindow = Just $ InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any
    }

denyExistence :: CardDef
denyExistence =
  (event "05032" "Deny Existence" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Paradox]
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ Discarded #when You source (basic AnyCard)
            , LostResources #when You source
            , LostActions #when You source
            , InvestigatorWouldTakeDamage #when You source
            , InvestigatorWouldTakeHorror #when You source
            ]
    }
 where
  source = SourceMatchesAny [SourceIsEnemyAttack AnyEnemy, Matcher.EncounterCardSource]

eldritchInspiration :: CardDef
eldritchInspiration =
  (event "05033" "Eldritch Inspiration" 0 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow =
        Just
          $ WouldTriggerChaosTokenRevealEffectOnCard
            You
            MysticCard
            [#skull, #cultist, #tablet, #elderthing, #autofail]
    , cdAlternateCardCodes = ["60420"]
    }

actOfDesperation :: CardDef
actOfDesperation =
  (event "05037" "Act of Desperation" 0 Survivor)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Tactic, Gambit]
    , cdAdditionalCost =
        Just $ DiscardFromCost 1 (FromHandOf You <> FromPlayAreaOf You) (#item <> CardFillsSlot HandSlot)
    }

crackTheCase :: CardDef
crackTheCase =
  (event "05110" "Crack the Case" 0 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DiscoveringLastClue #after You YourLocation
    , cdCriteria =
        Just $ exists $ affectsOthers $ can.gain.resources <> InvestigatorAt YourLocation
    }

intelReport :: CardDef
intelReport =
  (event "05111" "Intel Report" 2 Rogue)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Favor, Service]
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ Criteria.ClueOnLocation <> exists (You <> InvestigatorCanDiscoverCluesAt YourLocation)
            , Criteria.CanAffordCostIncrease 2
                <> exists
                  ( You
                      <> InvestigatorCanDiscoverCluesAt
                        (LocationMatchAny [LocationWithDistanceFrom n LocationWithAnyClues | n <- [0 .. 2]])
                  )
            ]
    , cdCardInHandEffects = True
    }

banish1 :: CardDef
banish1 =
  (event "05113" "Banish" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdActions = [#evade]
    , cdLevel = Just 1
    , cdCriteria = Just $ exists $ NonEliteEnemy <> CanEvadeEnemy ThisCard
    }

wellMaintained1 :: CardDef
wellMaintained1 =
  (event "05152" "Well-Maintained" 0 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = singleton Upgrade
    , cdLevel = Just 1
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy You
          <> #item
          <> NotAsset (AssetWithAttachedEvent $ EventIs "05152")
    }

swiftReflexes :: CardDef
swiftReflexes =
  (event "05156" "Swift Reflexes" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Gambit
    , cdCriteria = Just $ Criteria.Negate Criteria.DuringAction
    , cdFastWindow = Just $ DuringTurn Anyone
    }

bellyOfTheBeast :: CardDef
bellyOfTheBeast =
  (event "05160" "Belly of the Beast" 1 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Gambit, Trick]
    , cdFastWindow =
        Just
          $ SkillTestResult #after You (WhileEvadingAnEnemy AnyEnemy)
          $ SuccessResult
          $ atLeast 2
    , cdCriteria = Just $ exists $ YourLocation <> LocationWithDiscoverableCluesBy You
    }

warningShot :: CardDef
warningShot =
  (event "05229" "Warning Shot" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdAdditionalCost = Just $ UseCost (AssetWithTrait Firearm <> AssetControlledBy You) Uses.Ammo 1
    , cdCriteria = Just $ exists (EnemyAt YourLocation <> EnemyCanEnter ConnectedLocation)
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

telescopicSight3 :: CardDef
telescopicSight3 =
  (event "05230" "Telescopic Sight" 3 Guardian)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = setFromList [Item, Upgrade]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria = Just $ exists (AssetControlledBy You <> AssetInTwoHandSlots)
    , cdLevel = Just 3
    }

knowledgeIsPower :: CardDef
knowledgeIsPower =
  (event "05231" "Knowledge is Power" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists (AssetControlledBy You <> oneOf [AssetWithTrait Tome, AssetWithTrait Spell])
            , Criteria.ExtendedCardExists
                $ InHandOf You
                <> basic (oneOf [CardWithTrait Tome, CardWithTrait Spell] <> #asset)
                <> CardWithPerformableAbility
                  (AbilityOneOf [AbilityIsActionAbility, AbilityIsFastAbility])
                  [IgnoreAllCosts]
            ]
    }

decoy :: CardDef
decoy =
  (event "05234" "Decoy" 2 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Favor, Service]
    , cdActions = [#evade]
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists $ EnemyAt YourLocation <> CanEvadeEnemy ThisCard
            , Criteria.CanAffordCostIncrease 2
                <> exists
                  ( CanEvadeEnemyWithOverride
                      $ Criteria.CriteriaOverride
                      $ Criteria.EnemyCriteria
                      $ Criteria.EnemyExists
                      $ oneOf [EnemyAt (LocationWithDistanceFrom n Anywhere) | n <- [1 .. 2]]
                      <> NonEliteEnemy
                  )
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdCardInHandEffects = True
    }

fortuneOrFate2 :: CardDef
fortuneOrFate2 =
  (event "05237" "Fortune or Fate" 2 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdLimits = [MaxPerGame 1]
    , cdFastWindow = Just $ PlacedDoomCounter #when (SourceIsCancelable AnySource) ScenarioCardTarget
    , cdLevel = Just 2
    }

ghastlyRevelation :: CardDef
ghastlyRevelation =
  (event "05275" "Ghastly Revelation" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Spirit
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

smallFavor :: CardDef
smallFavor =
  (event "05277" "Small Favor" 2 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Favor, Service]
    , cdCriteria =
        Just
          $ Criteria.CanDealDamage
          <> Criteria.AnyCriterion
            [ exists $ EnemyAt YourLocation <> NonEliteEnemy
            , exists (oneOf [EnemyAt (LocationWithDistanceFrom n Anywhere) | n <- [1 .. 2]])
                <> Criteria.CanAffordCostIncrease 2
            ]
    , cdCardInHandEffects = True
    }

denyExistence5 :: CardDef
denyExistence5 =
  (event "05280" "Deny Existence" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Paradox]
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ Discarded #when You source (basic AnyCard)
            , LostResources #when You source
            , LostActions #when You source
            , InvestigatorWouldTakeDamage #when You source
            , InvestigatorWouldTakeHorror #when You source
            ]
    , cdLevel = Just 5
    }
 where
  source = SourceMatchesAny [SourceIsEnemyAttack AnyEnemy, Matcher.EncounterCardSource]

trialByFire :: CardDef
trialByFire =
  (event "05281" "Trial by Fire" 3 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    }

baitAndSwitch3 :: CardDef
baitAndSwitch3 =
  (event "05282" "Bait and Switch" 1 Survivor)
    { cdSkills = [#intellect, #agility, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#evade]
    , cdLevel = Just 3
    , cdCriteria =
        Just
          $ exists
          $ oneOf
            [ EnemyAt YourLocation <> CanEvadeEnemy ThisCard
            , CanEvadeEnemyWithOverride
                $ Criteria.CriteriaOverride
                $ Criteria.enemyExists
                $ EnemyAt (ConnectedFrom YourLocation)
                <> NonEliteEnemy
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

soothingMelody :: CardDef
soothingMelody =
  (event "05314" "Soothing Melody" 0 Guardian)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ Criteria.AnyCriterion
            [ exists (HealableInvestigator ThisCard DamageType $ InvestigatorAt YourLocation)
            , exists (HealableInvestigator ThisCard HorrorType $ InvestigatorAt YourLocation)
            , exists (HealableAsset ThisCard DamageType $ AssetAt YourLocation <> AllyAsset)
            , exists (HealableAsset ThisCard HorrorType $ AssetAt YourLocation <> AllyAsset)
            , Criteria.CanDrawCards
            ]
    , cdKeywords = setFromList [Keyword.Bonded 3 "05313", Keyword.Bonded 3 "54002"]
    , cdLevel = Nothing
    }

iveHadWorse2 :: CardDef
iveHadWorse2 =
  (event "05315" "\"I've had worse…\"" 0 Guardian)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DealtDamageOrHorror #when (SourceIsCancelable AnySource) You
    , cdLevel = Just 2
    }

bloodRite :: CardDef
bloodRite =
  (event "05317" "Blood-Rite" 0 Seeker)
    { cdSkills = [#willpower, #intellect, #combat]
    , cdCardTraits = singleton Spell
    , cdLevel = Nothing
    , cdKeywords = setFromList [Keyword.Bonded 3 "05316", Keyword.Bonded 3 "54004"]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

glimpseTheUnthinkable5 :: CardDef
glimpseTheUnthinkable5 =
  (event "05318" "Glimpse the Unthinkable" 1 Seeker)
    { cdSkills = [#intellect, #intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 5
    , cdCriteria = Just $ Criteria.AnyCriterion [Criteria.CanDrawCards, Criteria.CanManipulateDeck]
    }

youOweMeOne :: CardDef
youOweMeOne =
  (event "05319" "\"You owe me one!\"" 0 Rogue)
    { cdSkills = [#intellect, #combat, #agility]
    , cdCardTraits = setFromList [Favor, Gambit]
    , cdCriteria = Just $ exists (affectsOthers $ NotInvestigator You <> HandWith AnyCards)
    }

lure2 :: CardDef
lure2 =
  (event "05323" "Lure" 1 Survivor)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = singleton Trick
    , cdLevel = Just 2
    }

eucatastrophe3 :: CardDef
eucatastrophe3 =
  (event "05324" "Eucatastrophe" 2 Survivor)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdFastWindow = Just $ RevealChaosToken #when You WouldReduceYourSkillValueToZero
    , cdAlternateCardCodes = ["01692"]
    , cdLevel = Just 3
    }

occultEvidence :: CardDef
occultEvidence =
  signature "06002"
    $ (event "06008" "Occult Evidence" 0 Neutral)
      { cdSkills = [#wild]
      , cdCardTraits = setFromList [Insight, Research]
      , cdCardInSearchEffects = True
      , cdCardInHandEffects = True
      , cdCriteria = Just $ Criteria.CanManipulateDeck
      }

astoundingRevelation :: CardDef
astoundingRevelation =
  (event "06023" "Astounding Revelation" 0 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Research]
    , cdCost = Nothing
    , cdCardInSearchEffects = True
    , cdKeywords = singleton Keyword.Myriad
    }

easyMark1 :: CardDef
easyMark1 =
  (event "06026" "Easy Mark" 0 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdKeywords = singleton Keyword.Myriad
    , cdCriteria = Just $ Criteria.AnyCriterion [Criteria.CanGainResources, Criteria.CanDrawCards]
    , cdLevel = Just 1
    }

stargazing1 :: CardDef
stargazing1 =
  (event "06027" "Stargazing" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Insight, Augury]
    , cdLimits = [MaxPerGame 2]
    , cdCriteria = Just $ Criteria.EncounterDeckWith $ LengthIs (atLeast 10)
    , cdBondedWith = [(1, "06028")]
    , cdLevel = Just 1
    }

theStarsAreRight :: CardDef
theStarsAreRight =
  (event "06028" "The Stars Are Right" 0 Mystic)
    { cdCardTraits = singleton Augury
    , cdKeywords = singleton (Keyword.Bonded 1 "06027")
    , cdCost = Nothing
    , cdRevelation = IsRevelation
    , cdCardType = EncounterEventType
    , cdLevel = Nothing
    }

openGate :: CardDef
openGate =
  (event "06029" "Open Gate" 1 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorAt Anywhere)
          <> Criteria.EventCount (lessThan 3) (eventIs openGate)
    , cdFastWindow = Just $ DuringTurn You
    , cdKeywords = singleton Keyword.Myriad
    }

fortuitousDiscovery :: CardDef
fortuitousDiscovery =
  (event "06034" "Fortuitous Discovery" 0 Survivor)
    { cdCardTraits = setFromList [Fortune, Insight]
    , cdActions = [#investigate]
    , cdKeywords = singleton Keyword.Myriad
    , cdCost = Just DiscardAmountCost
    }

firstWatch :: CardDef
firstWatch =
  (event "06110" "First Watch" 1 Guardian)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

followed :: CardDef
followed =
  (event "06114" "Followed" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Tactic
    , cdActions = [#investigate]
    , cdCriteria = Just $ exists $ EnemyAt YourLocation
    , cdBeforeEffect = True
    }

readTheSigns :: CardDef
readTheSigns =
  (event "06117" "Read the Signs" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdActions = [#investigate]
    , cdCardTraits = setFromList [Spell]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

foolMeOnce1 :: CardDef
foolMeOnce1 =
  (event "06156" "\"Fool me once...\"" 1 Guardian)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdFastWindow =
        Just
          $ TreacheryWouldBeDiscarded #when
          $ TreacheryWithResolvedEffectsBy You
          <> TreacheryDiscardedBy You
    , cdLevel = Just 1
    }

letGodSortThemOut :: CardDef
letGodSortThemOut =
  (event "06160" "\"Let God sort them out...\"" 0 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Tactic, Fated]
    , cdCriteria = Just $ Criteria.HasHistory TurnHistory You $ DefeatedEnemiesWithTotalHealth (atLeast 6)
    }

swiftReload2 :: CardDef
swiftReload2 =
  (event "06161" "Swift Reload" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just $ DuringTurn You
    , cdCriteria =
        Just $ exists $ AssetControlledBy You <> AssetWithTrait Firearm <> AssetNotAtUsesX
    , cdLevel = Just 2
    }

etherealForm :: CardDef
etherealForm =
  (event "06164" "Ethereal Form" 2 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdActions = [#evade]
    , cdCardTraits = setFromList [Spell]
    }

scroungeForSupplies :: CardDef
scroungeForSupplies =
  (event "06165" "Scrounge for Supplies" 0 Survivor)
    { cdCardTraits = singleton Fortune
    , cdCriteria = Just $ Criteria.CardInDiscard (Criteria.DiscardOf You) (CardWithLevel 0)
    }

practiceMakesPerfect :: CardDef
practiceMakesPerfect =
  (event "06197" "Practice Makes Perfect" 1 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Gambit, Tactic]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just $ Criteria.DuringSkillTest SkillTestAtYourLocation
    }

extensiveResearch1 :: CardDef
extensiveResearch1 =
  (event "06198" "Extensive Research" 10 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 1
    }

spectralRazor :: CardDef
spectralRazor =
  (event "06201" "Spectral Razor" 2 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    , cdCriteria = Just $ exists $ oneOf [CanFightEnemy ThisCard, CanEngageEnemy ThisCard]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

wordOfCommand2 :: CardDef
wordOfCommand2 =
  (event "06202" "Word of Command" 2 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdLevel = Just 2
    , cdCriteria = can.manipulate.deck You
    }

lucidDreaming2 :: CardDef
lucidDreaming2 =
  (event "06205" "Lucid Dreaming" 1 Neutral)
    { cdCardTraits = setFromList [Spell]
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ youExist can.manipulate.deck
          <> exists (oneOf [InPlayAreaOf You, InHandOf (You <> can.reveal.cards) <> NotThisCard])
    }

heroicRescue2 :: CardDef
heroicRescue2 =
  (event "06234" "Heroic Rescue" 0 Guardian)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow =
        Just
          $ EnemyWouldAttack
            #when
            ( affectsOthers
                $ NotYou
                <> oneOf
                  [InvestigatorAt YourLocation, InvestigatorAt (CanMoveToLocation You ThisCard ConnectedLocation)]
            )
            AnyEnemyAttack
            NonEliteEnemy
    , cdLevel = Just 2
    }

aGlimmerOfHope :: CardDef
aGlimmerOfHope =
  (event "06245" "A Glimmer of Hope" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Blessed, Fortune]
    , cdKeywords = singleton Keyword.Myriad
    , cdCriteria = Just Criteria.InYourDiscard
    , cdPlayableFromDiscard = True
    }

nothingLeftToLose3 :: CardDef
nothingLeftToLose3 =
  (event "06284" "Nothing Left to Lose" 0 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdCriteria =
        Just
          $ exists
            (You <> oneOf [InvestigatorWithResources (lessThan 5), HandWith (LengthIs $ lessThan 5)])
    , cdLevel = Just 3
    }

obscureStudies :: CardDef
obscureStudies =
  signature "07002"
    $ (event "07008" "Obscure Studies" 0 Neutral)
      { cdSkills = [#wild, #wild, #wild]
      , cdCardTraits = singleton Insight
      , cdFastWindow = Just $ InitiatedSkillTest #when You #any #any #any
      }

inTheShadows :: CardDef
inTheShadows =
  signature "07003"
    $ (event "07010" "In the Shadows" 0 Neutral)
      { cdSkills = [#agility, #agility, #wild, #wild]
      , cdCardTraits = singleton Tactic
      , cdFastWindow = Just (TurnBegins #after You)
      }

handOfFate :: CardDef
handOfFate =
  (event "07020" "Hand of Fate" 3 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    }

deepKnowledge :: CardDef
deepKnowledge =
  (event "07023" "Deep Knowledge" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Cursed]
    , cdAdditionalCost = Just $ AddCurseTokenCost 2
    , cdCriteria =
        Just $ exists $ affectsOthers $ can.draw.cards FromPlayerCardEffect <> InvestigatorAt YourLocation
    }

faustianBargain :: CardDef
faustianBargain =
  (event "07028" "Faustian Bargain" 0 Rogue)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Pact, Cursed]
    , cdAdditionalCost = Just $ AddCurseTokenCost 2
    , cdCriteria =
        Just $ exists $ affectsOthers $ can.gain.resources <> InvestigatorAt YourLocation
    }

tidesOfFate :: CardDef
tidesOfFate =
  (event "07030" "Tides of Fate" 1 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdFastWindow = Just $ oneOf [FastPlayerWindow, RoundBegins #when]
    , cdCriteria = Just $ Criteria.ChaosTokenCountIs #bless (atLeast 1)
    }

wardOfRadiance :: CardDef
wardOfRadiance =
  (event "07031" "Ward of Radiance" 0 Mystic)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Insight, Blessed]
    , cdFastWindow =
        Just $ DrawCard #when Anyone (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
    }

keepFaith :: CardDef
keepFaith =
  (event "07034" "Keep Faith" 2 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just Criteria.HasRemainingBlessTokens
    }

temptFate :: CardDef
temptFate =
  (event "07037" "Tempt Fate" 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Fortune, Blessed, Cursed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just
          $ oneOf
            [ Criteria.HasRemainingBlessTokens
            , Criteria.HasRemainingCurseTokens
            , can.draw.cards FromPlayerCardEffect You
            ]
    }

righteousHunt1 :: CardDef
righteousHunt1 =
  (event "07109" "Righteous Hunt" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Blessed]
    , cdActions = [#engage]
    , cdLevel = Just 1
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdCriteria =
        Just
          $ exists
          $ CanEngageEnemyWithOverride
          $ Criteria.CriteriaOverride
          $ exists
          $ EnemyAt
          $ LocationWithAccessiblePath ThisCard 2 You Anywhere
    }

stirringUpTrouble1 :: CardDef
stirringUpTrouble1 =
  (event "07112" "Stirring Up Trouble" 0 Seeker)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = setFromList [Insight, Cursed]
    , cdAdditionalCost = Just AddCurseTokensEqualToShroudCost
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 1
    }

breakingAndEntering :: CardDef
breakingAndEntering =
  (event "07114" "Breaking and Entering" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#investigate]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    }

radiantSmite1 :: CardDef
radiantSmite1 =
  (event "07153" "Radiant Smite" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Spell, Blessed]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

theTruthBeckons :: CardDef
theTruthBeckons =
  (event "07154" "The Truth Beckons" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight]
    , cdCriteria =
        Just
          $ notExists EnemyEngagedWithYou
          <> exists (CanMoveCloserToLocation ThisCard You UnrevealedLocation)
    }

gazeOfOuraxsh2 :: CardDef
gazeOfOuraxsh2 =
  (event "07155" "Gaze of Ouraxsh" 2 Seeker)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spell, Cursed]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

underSurveillance1 :: CardDef
underSurveillance1 =
  (event "07157" "Under Surveillance" 3 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Tactic, Trap]
    , cdCriteria = Just $ Criteria.Negate $ exists $ "Under Surveillance" <> AssetAt YourLocation
    , cdLevel = Just 1
    }

butterflyEffect1 :: CardDef
butterflyEffect1 =
  (event "07160" "Butterfly Effect" 0 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Paradox, Blessed, Cursed]
    , cdFastWindow = Just $ RevealChaosToken #when (affectsOthers Anyone) IsSymbol
    , cdCriteria =
        Just
          $ Criteria.DuringSkillTest SkillTestAtYourLocation
          <> oneOf
            [ exists (CardIsCommittedBy (affectsOthers $ InvestigatorAt YourLocation))
            , exists (affectsOthers $ InvestigatorAt YourLocation <> InvestigatorWithCommittableCard)
            ]
    , cdLevel = Just 1
    }

thirdTimesACharm2 :: CardDef
thirdTimesACharm2 =
  (event "07161" "Third Time's a Charm" 1 Survivor)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow = Just $ InitiatedSkillTest #when (affectsOthers Anyone) #any #any #any
    , cdCriteria = Just $ Criteria.DuringSkillTest SkillTestAtYourLocation
    , cdLevel = Just 2
    }

manipulateDestiny2 :: CardDef
manipulateDestiny2 =
  (event "07162" "Manipulate Destiny" 1 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

riastrad1 :: CardDef
riastrad1 =
  (event "07193" "Ríastrad" 0 Rogue)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Spell, Spirit, Cursed]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

harmonyRestored2 :: CardDef
harmonyRestored2 =
  (event "07230" "Harmony Restored" 3 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdCriteria =
        Just
          $ Criteria.ChaosTokenCountIs #bless (atLeast 1)
          <> Criteria.ChaosTokenCountIs #curse (atLeast 1)
    , cdLevel = Just 2
    }

enchantWeapon3 :: CardDef
enchantWeapon3 =
  (event "07261" "Enchant Weapon" 3 Guardian)
    { cdSkills = [#willpower, #willpower, #combat]
    , cdCardTraits = setFromList [Spell, Upgrade]
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetWithTrait Weapon
          <> not_ (AssetWithAttachedEvent $ EventIs "07261")
    , cdLevel = Just 3
    }

theStygianEye3 :: CardDef
theStygianEye3 =
  (event "07263" "The Stygian Eye" 10 Seeker)
    { cdSkills = [#willpower, #willpower, #willpower]
    , cdCardTraits = setFromList [Insight, Cursed]
    , cdFastWindow = Just $ DuringTurn You
    , cdCardInHandEffects = True
    , cdLevel = Just 3
    }

aWatchfulPeace3 :: CardDef
aWatchfulPeace3 =
  (event "07269" "A Watchful Peace" 1 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Spirit, Blessed]
    , cdFastWindow = Just $ MythosStep WhenAllDrawEncounterCard
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    , cdAdditionalCost = Just $ ReturnChaosTokensToPoolCost 5 #bless
    , cdLevel = Just 3
    }

hallow3 :: CardDef
hallow3 =
  (event "07301" "Hallow" 3 Guardian)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdCriteria = Just Criteria.CardWithDoomExists
    , cdAdditionalCost = Just $ ReturnChaosTokensToPoolCost 10 (IncludeSealed #bless)
    , cdLevel = Just 3
    }

riteOfEquilibrium5 :: CardDef
riteOfEquilibrium5 =
  (event "07308" "Rite of Equilibrium" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Spell, Blessed, Cursed]
    , cdCriteria =
        Just
          $ oneOf
            [ Criteria.HasRemainingCurseTokens <> Criteria.HasRemainingBlessTokens
            , Criteria.ChaosTokenCountIs #curse (atLeast 1)
                <> Criteria.ChaosTokenCountIs #bless (atLeast 1)
                <> oneOf
                  [ exists (HealableAsset ThisCard #horror $ AssetAt YourLocation)
                  , exists (HealableInvestigator ThisCard #horror $ InvestigatorAt YourLocation)
                  ]
            ]
    , cdLevel = Just 5
    }

shrineOfTheMoirai3 :: CardDef
shrineOfTheMoirai3 =
  (event "07310" "Shrine of the Moirai" 1 Survivor)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Fortune, Blessed, Cursed]
    , cdCriteria = Just $ exists (LocationWithInvestigator You)
    , cdLevel = Just 3
    , cdUnique = True
    , cdUses = Uses.Uses Uses.Offering (Static 3)
    }

toeToToe :: CardDef
toeToToe =
  (event "08020" "Toe to Toe" 0 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdBeforeEffect = True
    }

getBehindMe :: CardDef
getBehindMe =
  (event "08021" "\"Get behind me!\"" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow = Just FastPlayerWindow
    }

sweepingKick1 :: CardDef
sweepingKick1 =
  (event "08023" "Sweeping Kick" 1 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic, Trick]
    , cdActions = [#fight]
    , cdLevel = Just 1
    }

dodge2 :: CardDef
dodge2 =
  (event "08026" "Dodge" 0 Guardian)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdLevel = Just 2
    }

unearthTheAncients2 :: CardDef
unearthTheAncients2 =
  (event "08039" "Unearth the Ancients" 0 Seeker)
    { cdSkills = [#intellect, #intellect, #agility]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    , cdCriteria = Just $ Criteria.ExtendedCardExists $ InHandOf You <> basic (#seeker <> #asset)
    , cdLevel = Just 2
    }

scoutAhead :: CardDef
scoutAhead =
  (event "08047" "Scout Ahead" 1 Rogue)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Insight, Trick]
    , cdActions = [#move]
    , cdCriteria = Just $ youExist can.move
    }

moneyTalks2 :: CardDef
moneyTalks2 =
  (event "08054" "Money Talks" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Favor, Gambit]
    , cdFastWindow =
        Just
          $ InitiatedSkillTest
            #when
            (affectsOthers $ InvestigatorAt Anywhere)
            AnySkillType
            AnySkillTestValue
            #any
    , cdLevel = Just 2
    }

meditativeTrance :: CardDef
meditativeTrance =
  (event "08061" "Meditative Trance" 2 Mystic)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Insight, Spirit]
    , cdCriteria =
        Just
          $ youExist
          $ InvestigatorWithFilledSlot #arcane
          <> oneOf [HealableInvestigator ThisCard dType You | dType <- [#damage, #horror]]
    }

parallelFates2 :: CardDef
parallelFates2 =
  (event "08066" "Parallel Fates" 0 Mystic)
    { cdSkills = [#willpower, #wild]
    , cdCardTraits = singleton Augury
    , cdLevel = Just 2
    , cdCriteria =
        Just $ exists $ oneOf [affectsOthers can.manipulate.deck, You <> can.target.encounterDeck]
    }

etherealSlip :: CardDef
etherealSlip =
  (multiClassEvent "08108" "Ethereal Slip" 2 [Rogue, Mystic])
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell, Trick]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> EnemyAt (LocationWithDistanceFromAtMost 2 YourLocation (RevealedLocation <> CanEnterLocation You))
          <> EnemyCanEnter YourLocation
    }

etherealSlip2 :: CardDef
etherealSlip2 =
  (multiClassEvent "08110" "Ethereal Slip" 1 [Rogue, Mystic])
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = setFromList [Spell, Trick]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> EnemyAt (RevealedLocation <> CanEnterLocation You)
          <> EnemyCanEnter YourLocation
    , cdLevel = Just 2
    }

breakingAndEntering2 :: CardDef
breakingAndEntering2 =
  (event "09074" "Breaking and Entering" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Trick]
    , cdActions = [#investigate]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

keepFaith2 :: CardDef
keepFaith2 =
  (event "10124" "Keep Faith" 0 Survivor)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Fortune, Blessed]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria = Just Criteria.HasRemainingBlessTokens
    , cdLevel = Just 2
    }

dynamiteBlast2 :: CardDef
dynamiteBlast2 =
  (event "50002" "Dynamite Blast" 4 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

barricade3 :: CardDef
barricade3 =
  (event "50004" "Barricade" 0 Seeker)
    { cdSkills = [#willpower, #intellect, #agility]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdLevel = Just 3
    }

hotStreak2 :: CardDef
hotStreak2 =
  (event "50006" "Hot Streak" 5 Rogue)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Fortune]
    , cdLevel = Just 2
    }

mindWipe3 :: CardDef
mindWipe3 =
  (event "50008" "Mind Wipe" 1 Mystic)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell]
    , cdLevel = Just 3
    , cdFastWindow = Just $ PhaseBegins #after AnyPhase
    }

preposterousSketches2 :: CardDef
preposterousSketches2 =
  (event "51003" "Preposterous Sketches" 0 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = singleton Insight
    , cdCriteria = Just Criteria.ClueOnLocation
    , cdLevel = Just 2
    }

contraband2 :: CardDef
contraband2 =
  (event "51005" "Contraband" 3 Rogue)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Supply, Illicit]
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetNotAtUseLimit
          <> oneOf [AssetWithUseType Uses.Ammo, AssetWithUseType Uses.Supply]
    }

thinkOnYourFeet2 :: CardDef
thinkOnYourFeet2 =
  (event "51006" "Think on Your Feet" 0 Rogue)
    { cdSkills = [#intellect, #agility, #agility]
    , cdCardTraits = singleton Trick
    , cdFastWindow = Just (EnemyEnters #when YourLocation AnyEnemy)
    , cdCriteria = Just $ exists AccessibleLocation <> exists (You <> can.move)
    , cdLevel = Just 2
    }

oops2 :: CardDef
oops2 =
  (event "51009" "Oops!" 2 Survivor)
    { cdSkills = [#combat, #combat, #agility]
    , cdCardTraits = singleton Fortune
    , cdLevel = Just 2
    , cdCriteria = Just Criteria.CanDealDamage
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileAttackingAnEnemy AnyEnemy) $ FailureResult $ lessThan 4
    }

eatLead :: CardDef
eatLead =
  (event "52002" "\"Eat lead!\"" 1 Guardian)
    { cdCardTraits = singleton Tactic
    , cdFastWindow =
        Just
          $ ActivateAbility
            #when
            You
            (AssetAbility (AssetWithTrait Firearm <> AssetWithUses Uses.Ammo) <> AbilityIsAction #fight)
    , cdSkills = [#combat, #agility]
    }

logicalReasoning4 :: CardDef
logicalReasoning4 =
  (event "52003" "Logical Reasoning" 2 Seeker)
    { cdSkills = [#willpower, #willpower, #willpower]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 4
    , cdCriteria =
        Just
          $ exists (You <> InvestigatorWithAnyClues)
          <> Criteria.AnyCriterion
            [ exists $ HealableInvestigator ThisCard HorrorType $ InvestigatorAt YourLocation
            , exists
                $ TreacheryWithTrait Terror
                <> TreacheryInThreatAreaOf (affectsOthers $ InvestigatorAt YourLocation)
            ]
    }

stormOfSpirits3 :: CardDef
stormOfSpirits3 =
  (event "52008" "Storm of Spirits" 3 Mystic)
    { cdSkills = [#willpower, #combat, #combat]
    , cdCardTraits = singleton Spell
    , cdActions = [#fight]
    , cdLevel = Just 3
    }

bloodEclipse1 :: CardDef
bloodEclipse1 =
  (event "53001" "Blood Eclipse" 1 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdActions = [#fight]
    , cdAdditionalCost = Just $ InvestigatorDamageCost ThisCard You DamageAny 2
    , cdLevel = Just 1
    }

truthFromFiction2 :: CardDef
truthFromFiction2 =
  (event "53003" "Truth from Fiction" 1 Seeker)
    { cdSkills = [#intellect, #intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> AssetCanHaveUses Uses.Secret
    }

alterFate1 :: CardDef
alterFate1 =
  (event "53009" "Alter Fate" 3 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Spell, Blessed]
    , cdCriteria =
        Just $ exists $ NotTreachery (TreacheryOnEnemy EliteEnemy) <> TreacheryIsNonWeakness
    , cdLevel = Just 1
    }

trialByFire3 :: CardDef
trialByFire3 =
  (event "54010" "Trial by Fire" 2 Survivor)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 3
    }

cleanThemOut :: CardDef
cleanThemOut =
  (event "60111" "Clean Them Out" 0 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#willpower, #combat]
    }

counterpunch :: CardDef
counterpunch =
  (event "60112" "Counterpunch" 0 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#combat, #agility]
    , cdFastWindow = Just $ EnemyAttacksEvenIfCancelled #after You AnyEnemyAttack AnyEnemy
    }

-- We need to override the action check for this card because of multiple actions,
-- but even if we can not fight or engage the enemy, if we can move it this should
-- still be playable
getOverHere :: CardDef
getOverHere =
  (event "60114" "\"Get over here!\"" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#engage, #fight]
    , cdSkills = [#willpower, #combat]
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> oneOf
            [ EnemyAt YourLocation <> oneOf [CanEngageEnemy ThisCard, CanFightEnemy ThisCard]
            , EnemyAt $ ConnectedFrom YourLocation
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    }

glory :: CardDef
glory =
  (event "60115" "Glory" 1 Guardian)
    { cdCardTraits = singleton Spirit
    , cdSkills = [#intellect, #intellect]
    , cdFastWindow = Just $ EnemyDefeated #after You ByAny AnyEnemy
    }

monsterSlayer :: CardDef
monsterSlayer =
  (event "60116" "Monster Slayer" 0 Guardian)
    { cdCardTraits = singleton Spirit
    , cdActions = [#fight]
    , cdSkills = [#wild]
    }

oneTwoPunch :: CardDef
oneTwoPunch =
  (event "60117" "One-Two Punch" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#combat]
    }

standTogether :: CardDef
standTogether =
  (event "60118" "Stand Together" 0 Guardian)
    { cdCardTraits = singleton Spirit
    , cdSkills = [#willpower]
    , cdCriteria =
        Just
          $ exists (affectsOthers $ NotYou <> InvestigatorAt YourLocation)
          <> exists (affectsOthers $ InvestigatorAt YourLocation <> can.gain.resources)
    }

evidence1 :: CardDef
evidence1 =
  (event "60120" "Evidence!" 1 Guardian)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just (EnemyDefeated #after You ByAny AnyEnemy)
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 1
    }

galvanize1 :: CardDef
galvanize1 =
  (event "60121" "Galvanize" 2 Guardian)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 1
    }

counterpunch2 :: CardDef
counterpunch2 =
  (event "60122" "Counterpunch" 0 Guardian)
    { cdSkills = [#combat, #combat, #agility]
    , cdCardTraits = setFromList [Spirit, Tactic]
    , cdFastWindow = Just $ EnemyAttacks #when You AnyEnemyAttack AnyEnemy
    , cdLevel = Just 2
    }

-- We need to override the action check for this card because of multiple actions,
-- but even if we can not fight or engage the enemy, if we can move it this should
-- still be playable
getOverHere2 :: CardDef
getOverHere2 =
  (event "60123" "\"Get over here!\"" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#engage, #fight]
    , cdSkills = [#willpower, #willpower, #combat]
    , cdFastWindow = Just FastPlayerWindow
    , cdCriteria =
        Just
          $ exists
          $ NonEliteEnemy
          <> oneOf
            [ EnemyAt YourLocation <> oneOf [CanEngageEnemy ThisCard, CanFightEnemy ThisCard]
            , EnemyAt $ ConnectedFrom YourLocation
            , EnemyAt $ LocationWithDistanceFrom 2 YourLocation
            ]
    , cdOverrideActionPlayableIfCriteriaMet = True
    , cdLevel = Just 2
    }

lessonLearned2 :: CardDef
lessonLearned2 =
  (event "60124" "Lesson Learned" 1 Guardian)
    { cdCardTraits = setFromList [Insight, Spirit]
    , cdSkills = [#willpower, #intellect, #intellect]
    , cdFastWindow = Just $ DealtDamage #after (SourceIsEnemyAttack AnyEnemy) You
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    , cdLevel = Just 2
    }

manoAMano2 :: CardDef
manoAMano2 =
  (event "60125" "Mano a Mano" 0 Guardian)
    { cdSkills = [#willpower, #combat]
    , cdCardTraits = setFromList [Spirit, Bold]
    , cdCriteria =
        Just $ Criteria.FirstAction <> exists EnemyEngagedWithYou <> Criteria.CanDealDamage
    , cdAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
    , cdLevel = Just 2
    }

dynamiteBlast3 :: CardDef
dynamiteBlast3 =
  (event "60129" "Dynamite Blast" 4 Guardian)
    { cdSkills = [#willpower, #willpower, #combat, #combat]
    , cdCardTraits = setFromList [Tactic]
    , cdLevel = Just 3
    }

taunt3 :: CardDef
taunt3 =
  (event "60130" "Taunt" 1 Guardian)
    { cdCardTraits = setFromList [Tactic]
    , cdFastWindow = Just $ DuringTurn You
    , cdSkills = [#willpower, #willpower, #combat, #agility]
    , cdLevel = Just 3
    }

oneTwoPunch5 :: CardDef
oneTwoPunch5 =
  (event "60132" "One-Two Punch" 2 Guardian)
    { cdCardTraits = setFromList [Spirit, Tactic]
    , cdActions = [#fight]
    , cdSkills = [#combat, #combat, #combat, #combat]
    , cdLevel = Just 5
    }

burningTheMidnightOil :: CardDef
burningTheMidnightOil =
  (event "60214" "Burning the Midnight Oil" 0 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Insight
    , cdActions = [#investigate]
    }

crypticWritings :: CardDef
crypticWritings =
  (event "60215" "Cryptic Writings" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    }

extensiveResearch :: CardDef
extensiveResearch =
  (event "60216" "Extensive Research" 12 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdCriteria = Just canDiscoverCluesAtYourLocation
    }

occultInvocation :: CardDef
occultInvocation =
  (event "60217" "Occult Invocation" 2 Seeker)
    { cdSkills = [#combat, #intellect]
    , cdCardTraits = singleton Spell
    , cdAdditionalCost = Just $ UpTo 2 $ HandDiscardCost 1 AnyCard
    , cdActions = [#fight]
    }

glimpseTheUnthinkable1 :: CardDef
glimpseTheUnthinkable1 =
  (event "60221" "Glimpse the Unthinkable" 0 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 1
    , cdCriteria = Just $ Criteria.AnyCriterion [Criteria.CanDrawCards, Criteria.CanManipulateDeck]
    }

crypticWritings2 :: CardDef
crypticWritings2 =
  (event "60224" "Cryptic Writings" 0 Seeker)
    { cdSkills = [#intellect, #wild]
    , cdCardTraits = singleton Insight
    , cdCardInHandEffects = True
    , cdLevel = Just 2
    }

iveGotAPlan2 :: CardDef
iveGotAPlan2 =
  (event "60225" "\"I've got a plan!\"" 2 Seeker)
    { cdSkills = [#intellect, #intellect, #combat]
    , cdCardTraits = setFromList [Insight, Tactic]
    , cdLevel = Just 2
    , cdActions = [#fight]
    }

mindOverMatter2 :: CardDef
mindOverMatter2 =
  (event "60226" "Mind over Matter" 1 Seeker)
    { cdSkills = [#combat, #agility, #wild]
    , cdCardTraits = singleton Insight
    , cdFastWindow = Just $ DuringTurn You
    , cdLevel = Just 2
    }

seekingAnswers2 :: CardDef
seekingAnswers2 =
  (event "60227" "Seeking Answers" 1 Seeker)
    { cdSkills = [#intellect, #agility, #agility]
    , cdActions = [#investigate]
    , cdCardTraits = singleton Insight
    , cdLevel = Just 2
    , cdAlternateCardCodes = ["01685"]
    }

pilfer :: CardDef
pilfer =
  (event "60315" "Pilfer" 4 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdActions = [#investigate]
    }

sneakBy :: CardDef
sneakBy =
  (event "60316" "Sneak By" 0 Rogue)
    { cdCardTraits = singleton Trick
    , cdActions = [#evade]
    , cdSkills = [#agility, #agility]
    }

daringManeuver2 :: CardDef
daringManeuver2 =
  (event "60322" "Daring Maneuver" 0 Rogue)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Gambit
    , cdFastWindow = Just $ WouldHaveSkillTestResult #when You AnySkillTest $ SuccessResult AnyValue
    , cdLevel = Just 2
    }

cheapShot2 :: CardDef
cheapShot2 =
  (event "60323" "Cheap Shot" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = singleton Trick
    , cdActions = [#fight]
    , cdLevel = Just 2
    }

slipAway2 :: CardDef
slipAway2 =
  (event "60324" "Slip Away" 2 Rogue)
    { cdCardTraits = singleton Trick
    , cdSkills = [#intellect, #agility]
    , cdActions = [#evade]
    , cdLevel = Just 2
    }

pilfer3 :: CardDef
pilfer3 =
  (event "60328" "Pilfer" 4 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = singleton Trick
    , cdActions = [#investigate]
    , cdLevel = Just 3
    }

backstab3 :: CardDef
backstab3 =
  (event "60329" "Backstab" 3 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic]
    , cdActions = [#fight]
    , cdLevel = Just 3
    }

parallelFates :: CardDef
parallelFates =
  (event "60415" "Parallel Fates" 0 Mystic)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Augury
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

voiceOfRa :: CardDef
voiceOfRa =
  (event "60416" "Voice of Ra" 0 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spell
    , cdCriteria = Just $ exists $ can.gain.resources <> You
    }

eldritchInspiration1 :: CardDef
eldritchInspiration1 =
  (event "60420" "Eldritch Inspiration" 0 Mystic)
    { cdSkills = [#willpower, #intellect, #intellect]
    , cdCardTraits = setFromList [Spell, Spirit]
    , cdFastWindow = Just $ WouldTriggerChaosTokenRevealEffectOnCard You MysticCard [minBound ..]
    , cdLevel = Just 1
    }

hypnoticGaze2 :: CardDef
hypnoticGaze2 =
  (event "60423" "Hypnotic Gaze" 2 Mystic)
    { cdSkills = [#combat, #agility, #agility]
    , cdCardTraits = singleton Spell
    , cdFastWindow =
        Just
          $ EnemyAttacks
            #when
            (affectsOthers $ InvestigatorAt YourLocation)
            (CancelableEnemyAttack AnyEnemyAttack)
            AnyEnemy
    , cdLevel = Just 2
    }

recharge4 :: CardDef
recharge4 =
  (event "60429" "Recharge" 0 Mystic)
    { cdSkills = [#willpower, #willpower, #willpower]
    , cdCardTraits = singleton Spell
    , cdCriteria =
        Just
          $ exists
          $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
          <> oneOf [AssetWithTrait Spell, AssetWithTrait Relic]
    , cdLevel = Just 4
    }

willToSurvive :: CardDef
willToSurvive =
  (event "60512" "Will to Survive" 4 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Spirit]
    , cdFastWindow = Just (DuringTurn You)
    }

aTestOfWill :: CardDef
aTestOfWill =
  (event "60513" "A Test of Will" 1 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ DrawCard
                #when
                (InvestigatorAt YourLocation)
                (basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard #when You (basic NonWeaknessTreachery) EncounterDeck
            ]
    }

gritYourTeeth :: CardDef
gritYourTeeth =
  (event "60515" "Grit Your Teeth" 1 Survivor)
    { cdSkills = [#wild]
    , cdCardTraits = singleton Spirit
    , cdFastWindow = Just $ SkillTestResult #after You AnySkillTest $ FailureResult AnyValue
    }

aTestOfWill2 :: CardDef
aTestOfWill2 =
  (event "60523" "A Test of Will" 0 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = singleton Spirit
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ DrawCard
                #when
                (affectsOthers $ InvestigatorAt YourLocation)
                (CanCancelRevelationEffect $ basic $ NonPeril <> NonWeaknessTreachery)
                EncounterDeck
            , DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck
            ]
    , cdLevel = Just 2
    }

lookWhatIFound2 :: CardDef
lookWhatIFound2 =
  (event "60524" "\"Look what I found!\"" 2 Survivor)
    { cdSkills = [#intellect, #intellect, #agility]
    , cdCardTraits = singleton Fortune
    , cdLevel = Just 2
    , cdCriteria =
        Just
          $ Criteria.Criteria
            [ exists $ LocationMatchAny [YourLocation, ConnectedLocation] <> LocationWithAnyClues
            , exists
                $ You
                <> InvestigatorCanDiscoverCluesAt (LocationMatchAny [YourLocation, ConnectedLocation])
            ]
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileInvestigating Anywhere) $ FailureResult $ lessThan 4
    }

dumbLuck2 :: CardDef
dumbLuck2 =
  (event "60525" "Dumb Luck" 2 Survivor)
    { cdSkills = [#willpower, #agility, #agility]
    , cdCardTraits = singleton Fortune
    , cdFastWindow =
        Just $ SkillTestResult #after You (WhileEvadingAnEnemy NonEliteEnemy) $ FailureResult $ lessThan 4
    , cdLevel = Just 2
    , cdCriteria = Just $ exists $ You <> can.target.encounterDeck
    }

lucky3 :: CardDef
lucky3 =
  (event "60528" "Lucky!" 0 Survivor)
    { cdCardTraits = singleton Fortune
    , cdFastWindow =
        Just
          $ WouldHaveSkillTestResult #when (affectsOthers $ InvestigatorAt YourLocation) AnySkillTest
          $ FailureResult AnyValue
    , cdLevel = Just 3
    }

mysteriesRemain :: CardDef
mysteriesRemain =
  signature "01001"
    $ (event "98005" "Mysteries Remain" 0 Neutral)
      { cdSkills = [#combat, #intellect, #wild]
      , cdCardTraits = singleton Insight
      , cdKeywords = setFromList [Keyword.Replacement]
      , cdFastWindow = Just $ DuringTurn You
      , cdCriteria = Just $ youExist (at_ Anywhere)
      }
