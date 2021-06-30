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

import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

genPlayerCard :: MonadRandom m => CardCode -> m PlayerCard
genPlayerCard cardCode = do
  cardId <- getRandom
  pure $ MkPlayerCard
    { pcId = cardId
    , pcDef = lookupPlayerCardDef cardCode
    , pcBearer = Nothing
    }

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName =
  mkName . pcName . lookupPlayerCardDef

lookupPlayerCard :: CardCode -> CardId -> PlayerCard
lookupPlayerCard cardCode cardId = MkPlayerCard
  { pcId = cardId
  , pcDef = lookupPlayerCardDef cardCode
  , pcBearer = Nothing
  }

lookupPlayerCardDef :: CardCode -> PlayerCardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode PlayerCardDef
allPlayerCards = mapFromList
  [ ("asset", placeholderAsset)
  , ("01006", rolands38Special)
  , ("01007", coverUp)
  , ("01008", daisysToteBag)
  , ("01009", theNecronomicon)
  , ("01010", onTheLam)
  , ("01011", hospitalDebts)
  , ("01012", heirloomOfHyperborea)
  , ("01013", darkMemory)
  , ("01014", wendysAmulet)
  , ("01015", abandonedAndAlone)
  , ("01016", fortyFiveAutomatic)
  , ("01017", physicalTraining)
  , ("01018", beatCop)
  , ("01019", firstAid)
  , ("01020", machete)
  , ("01021", guardDog)
  , ("01022", evidence)
  , ("01023", dodge)
  , ("01024", dynamiteBlast)
  , ("01025", viciousBlow)
  , ("01026", extraAmmunition1)
  , ("01027", policeBadge2)
  , ("01028", beatCop2)
  , ("01029", shotgun4)
  , ("01030", magnifyingGlass)
  , ("01031", oldBookOfLore)
  , ("01032", researchLibrarian)
  , ("01033", drMilanChristopher)
  , ("01034", hyperawareness)
  , ("01035", medicalTexts)
  , ("01036", mindOverMatter)
  , ("01037", workingAHunch)
  , ("01038", barricade)
  , ("01039", deduction)
  , ("01040", magnifyingGlass1)
  , ("01041", discOfItzamna2)
  , ("01042", encyclopedia2)
  , ("01043", crypticResearch4)
  , ("01044", switchblade)
  , ("01045", burglary)
  , ("01046", pickpoketing)
  , ("01047", fortyOneDerringer)
  , ("01048", leoDeLuca)
  , ("01049", hardKnocks)
  , ("01050", elusive)
  , ("01051", backstab)
  , ("01052", sneakAttack)
  , ("01053", opportunist)
  , ("01054", leoDeLuca1)
  , ("01055", catBurgler1)
  , ("01056", sureGamble3)
  , ("01057", hotStreak4)
  , ("01058", forbiddenKnowledge)
  , ("01059", holyRosary)
  , ("01060", shrivelling)
  , ("01061", scrying)
  , ("01062", arcaneStudies)
  , ("01063", arcaneInitiate)
  , ("01064", drawnToTheFlame)
  , ("01065", wardOfProtection)
  , ("01066", blindingLight)
  , ("01067", fearless)
  , ("01068", mindWipe1)
  , ("01069", blindingLight2)
  , ("01070", bookOfShadows3)
  , ("01071", grotesqueStatue4)
  , ("01072", leatherCoat)
  , ("01073", scavenging)
  , ("01074", baseballBat)
  , ("01075", rabbitsFoot)
  , ("01076", strayCat)
  , ("01077", digDeep)
  , ("01078", cunningDistraction)
  , ("01079", lookWhatIFound)
  , ("01080", lucky)
  , ("01081", survivalInstinct)
  , ("01082", aquinnah1)
  , ("01083", closeCall2)
  , ("01084", lucky2)
  , ("01085", willToSurvive4)
  , ("01086", knife)
  , ("01087", flashlight)
  , ("01088", emergencyCache)
  , ("01089", guts)
  , ("01090", perception)
  , ("01091", overpower)
  , ("01092", manualDexterity)
  , ("01093", unexpectedCourage)
  , ("01094", bulletproofVest3)
  , ("01095", elderSignAmulet3)
  , ("01096", amnesia)
  , ("01097", paranoia)
  , ("01098", haunted)
  , ("01099", psychosis)
  , ("01100", hypochondria)
  , ("01101", mobEnforcer)
  , ("01102", silverTwilightAcolyte)
  , ("01103", stubbornDetective)
  , ("01117", litaChantler)
  , ("02006", zoeysCross)
  , ("02007", smiteTheWicked)
  , ("02008", searchForTheTruth)
  , ("02009", rexsCurse)
  , ("02010", jennysTwin45s)
  , ("02011", searchingForIzzie)
  , ("02012", jimsTrumpet)
  , ("02013", finalRhapsody)
  , ("02014", duke)
  , ("02015", wrackedByNightmares)
  , ("02016", blackjack)
  , ("02017", taunt)
  , ("02018", teamwork)
  , ("02019", taunt2)
  , ("02020", laboratoryAssistant)
  , ("02021", strangeSolution)
  , ("02022", shortcut)
  , ("02023", seekingAnswers)
  , ("02024", liquidCourage)
  , ("02025", thinkOnYourFeet)
  , ("02026", doubleOrNothing)
  , ("02027", hiredMuscle1)
  , ("02028", riteOfSeeking)
  , ("02029", ritualCandles)
  , ("02030", clarityOfMind)
  , ("02031", bindMonster)
  , ("02032", fireAxe)
  , ("02033", peterSylvestre)
  , ("02034", baitAndSwitch)
  , ("02035", peterSylvestre2)
  , ("02036", kukri)
  , ("02037", indebted)
  , ("02038", internalInjury)
  , ("02039", chronophobia)
  , ("02040", drHenryArmitage)
  , ("02059", alchemicalConcoction)
  , ("02060", jazzMulligan)
  , ("02061", professorWarrenRice)
  , ("02079", peterClover)
  , ("02080", drFrancisMorgan)
  , ("02105", emergencyAid)
  , ("02106", brotherXavier1)
  , ("02107", iveGotAPlan)
  , ("02108", pathfinder1)
  , ("02109", contraband)
  , ("02110", adaptable1)
  , ("02111", delveTooDeep)
  , ("02112", songOfTheDead2)
  , ("02139", adamLynch)
  , ("02140", theNecronomiconOlausWormiusTranslation)
  , ("02147", bandolier)
  , ("02178", acrossSpaceAndTime)
  , ("02179", helplessPassenger)
  , ("02185", keenEye3)
  , ("02215", keyToTheChamber)
  , ("02217", zebulonWhateley)
  , ("02218", earlSawyer)
  , ("02219", powderOfIbnGhazi)
  , ("02226", springfieldM19034)
  , ("02254", esotericFormula)
  , ("02301", lightningGun5)
  , ("03022", letMeHandleThis)
  , ("04023", toothOfEztli)
  , ("04149", secondWind)
  , ("04153", trueUnderstanding)
  , ("05316", occultLexicon)
  , ("05317", bloodRite)
  , ("06023", astoundingRevelation)
  , ("06110", firstWatch)
  , ("06116", scrollOfProphecies)
  , ("07152", keenEye)
  , ("50001", physicalTraining2)
  , ("50002", dynamiteBlast2)
  , ("50003", hyperawareness2)
  , ("50004", barricade3)
  , ("50005", hardKnocks2)
  , ("50006", hotStreak2)
  , ("50007", arcaneStudies2)
  , ("50008", mindWipe3)
  , ("50009", digDeep2)
  , ("50010", rabbitsFoot3)
  , ("51005", contraband2)
  , ("60130", taunt3)
  , ("60205", arcaneEnlightenment)
  , ("60206", celaenoFragments)
  , ("60208", encyclopedia)
  , ("60211", higherEducation)
  , ("60213", whittonGreene)
  , ("60225", iveGotAPlan2)
  , ("60504", atychiphobia)
  , ("81019", ladyEsprit)
  , ("81020", bearTrap)
  , ("81021", fishingNet)
  , ("81029", curseOfTheRougarou)
  , ("81030", monstrousTransformation)
  , ("90002", daisysToteBagAdvanced)
  , ("90003", theNecronomiconAdvanced)
  ]

basePlayerCard
  :: CardCode
  -> Text
  -> Int
  -> PlayerCardType
  -> ClassSymbol
  -> PlayerCardDef
basePlayerCard cardCode name cost cardType classSymbol = PlayerCardDef
  { pcCardCode = cardCode
  , pcName = name
  , pcCost = StaticCost cost
  , pcLevel = 0
  , pcCardType = cardType
  , pcWeakness = False
  , pcClassSymbol = classSymbol
  , pcSkills = mempty
  , pcTraits = mempty
  , pcKeywords = mempty
  , pcFast = False
  , pcWindows = mempty
  , pcAction = Nothing
  , pcRevelation = False
  , pcVictoryPoints = Nothing
  , pcCommitRestrictions = mempty
  , pcAttackOfOpportunityModifiers = mempty
  , pcPermanent = False
  }

asset :: CardCode -> Text -> Int -> ClassSymbol -> PlayerCardDef
asset cardCode name cost classSymbol =
  basePlayerCard cardCode name cost AssetType classSymbol

event :: CardCode -> Text -> Int -> ClassSymbol -> PlayerCardDef
event cardCode name cost classSymbol =
  basePlayerCard cardCode name cost EventType classSymbol

skill :: CardCode -> Text -> [SkillType] -> ClassSymbol -> PlayerCardDef
skill cardCode name skills classSymbol =
  (basePlayerCard cardCode name 0 SkillType classSymbol)
    { pcSkills = skills
    }

treachery :: CardCode -> Text -> Int -> PlayerCardDef
treachery cardCode name cost =
  (basePlayerCard cardCode name cost PlayerTreacheryType Neutral)
    { pcWeakness = True
    }

enemy :: CardCode -> Text -> Int -> PlayerCardDef
enemy cardCode name cost =
  (basePlayerCard cardCode name cost PlayerEnemyType Neutral)
    { pcWeakness = True
    }

placeholderAsset :: PlayerCardDef
placeholderAsset = asset "asset" "Placeholder Asset" 0 Neutral

rolands38Special :: PlayerCardDef
rolands38Special =
  (asset "01006" "Roland's .38 Special" 3 Neutral)
    { pcSkills = [SkillCombat, SkillAgility, SkillWild]
    , pcTraits = setFromList [Item, Weapon, Firearm]
    }

coverUp :: PlayerCardDef
coverUp = (treachery "01007" "Cover Up" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

daisysToteBag :: PlayerCardDef
daisysToteBag = (asset "01008" "Daisy's Tote Bag" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = setFromList [Item]
  }

theNecronomicon :: PlayerCardDef
theNecronomicon = (asset "01009" "The Necronomicon" 0 Neutral)
  { pcTraits = setFromList [Item, Tome]
  , pcWeakness = True
  , pcRevelation = True
  }

onTheLam :: PlayerCardDef
onTheLam = (event "01010" "On the Lam" 1 Neutral)
  { pcTraits = setFromList [Tactic]
  , pcSkills = [SkillIntellect, SkillAgility, SkillWild, SkillWild]
  , pcFast = True
  , pcWindows = setFromList [AfterTurnBegins You, DuringTurn You]
  }

hospitalDebts :: PlayerCardDef
hospitalDebts = (treachery "01011" "Hospital Debts" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

heirloomOfHyperborea :: PlayerCardDef
heirloomOfHyperborea =
  (asset "01012" "Heirloom of Hyperborea" 3 Neutral)
    { pcSkills = [SkillWillpower, SkillCombat, SkillWild]
    , pcTraits = setFromList [Item, Relic]
    }

darkMemory :: PlayerCardDef
darkMemory = (event "01013" "Dark Memory" 2 Neutral)
  { pcTraits = setFromList [Spell]
  , pcWeakness = True
  }

wendysAmulet :: PlayerCardDef
wendysAmulet = (asset "01014" "Wendy's Amulet" 2 Neutral)
  { pcSkills = [SkillWild, SkillWild]
  , pcTraits = setFromList [Item, Relic]
  }

abandonedAndAlone :: PlayerCardDef
abandonedAndAlone = (treachery "01015" "Abandoned and Alone" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

fortyFiveAutomatic :: PlayerCardDef
fortyFiveAutomatic = (asset "01016" ".45 Automatic" 4 Guardian)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Item, Weapon, Firearm]
  }

physicalTraining :: PlayerCardDef
physicalTraining = (asset "01017" "Physical Training" 2 Guardian)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Talent]
  }

beatCop :: PlayerCardDef
beatCop = (asset "01018" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Ally, Police]
  }

firstAid :: PlayerCardDef
firstAid = (asset "01019" "First Aid" 2 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Talent, Science]
  }

machete :: PlayerCardDef
machete = (asset "01020" "Machete" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

guardDog :: PlayerCardDef
guardDog = (asset "01021" "Guard Dog" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Ally, Creature]
  }

evidence :: PlayerCardDef
evidence = (event "01022" "Evidence!" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Insight]
  , pcFast = True
  , pcWindows = setFromList [WhenEnemyDefeated You]
  }

dodge :: PlayerCardDef
dodge = (event "01023" "Dodge" 1 Guardian)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [WhenEnemyAttacks InvestigatorAtYourLocation]
  }

dynamiteBlast :: PlayerCardDef
dynamiteBlast = (event "01024" "Dynamite Blast" 5 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Tactic]
  }

viciousBlow :: PlayerCardDef
viciousBlow =
  (skill "01025" "Vicious Blow" [SkillCombat] Guardian)
    { pcTraits = setFromList [Practiced]
    }

extraAmmunition1 :: PlayerCardDef
extraAmmunition1 = (event "01026" "Extra Ammunition" 2 Guardian)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Supply]
  , pcLevel = 1
  }

policeBadge2 :: PlayerCardDef
policeBadge2 = (asset "01027" "Police Badge" 3 Guardian)
  { pcSkills = [SkillWillpower, SkillWild]
  , pcTraits = setFromList [Item]
  , pcLevel = 2
  }

beatCop2 :: PlayerCardDef
beatCop2 = (asset "01028" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Ally, Police]
  , pcLevel = 2
  }

shotgun4 :: PlayerCardDef
shotgun4 = (asset "01029" "Shotgun" 5 Guardian)
  { pcSkills = [SkillCombat, SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Firearm]
  , pcLevel = 4
  }

magnifyingGlass :: PlayerCardDef
magnifyingGlass = (asset "01030" "Magnifying Glass" 1 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tool]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

oldBookOfLore :: PlayerCardDef
oldBookOfLore = (asset "01031" "Old Book of Lore" 3 Seeker)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Tome]
  }

researchLibrarian :: PlayerCardDef
researchLibrarian = (asset "01032" "Research Librarian" 2 Seeker)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

drMilanChristopher :: PlayerCardDef
drMilanChristopher =
  (asset "01033" "Dr. Milan Christopher" 4 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Ally, Miskatonic]
    }

hyperawareness :: PlayerCardDef
hyperawareness = (asset "01034" "Hyperawareness" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Talent]
  }

medicalTexts :: PlayerCardDef
medicalTexts = (asset "01035" "Medical Texts" 2 Seeker)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Tome]
  }

mindOverMatter :: PlayerCardDef
mindOverMatter = (event "01036" "Mind over Matter" 1 Seeker)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Insight]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

workingAHunch :: PlayerCardDef
workingAHunch = (event "01037" "Working a Hunch" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Insight]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

barricade :: PlayerCardDef
barricade = (event "01038" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Insight, Tactic]
  }

deduction :: PlayerCardDef
deduction = (skill "01039" "Deduction" [SkillIntellect] Seeker)
  { pcTraits = setFromList [Practiced]
  }

magnifyingGlass1 :: PlayerCardDef
magnifyingGlass1 = (asset "01040" "Magnifying Glass" 0 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tool]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcLevel = 1
  }

discOfItzamna2 :: PlayerCardDef
discOfItzamna2 = (asset "01041" "Disc of Itzamna" 3 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Item, Relic]
  , pcLevel = 2
  }

encyclopedia2 :: PlayerCardDef
encyclopedia2 = (asset "01042" "Encyclopedia" 2 Seeker)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Tome]
  , pcLevel = 2
  }

crypticResearch4 :: PlayerCardDef
crypticResearch4 = (event "01043" "Cryptic Research" 0 Seeker)
  { pcTraits = setFromList [Insight]
  , pcLevel = 4
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

switchblade :: PlayerCardDef
switchblade = (asset "01044" "Switchbalde" 1 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Item, Weapon, Melee, Illicit]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

burglary :: PlayerCardDef
burglary = (asset "01045" "Burglary" 1 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Talent, Illicit]
  }

pickpoketing :: PlayerCardDef
pickpoketing = (asset "01046" "Pickpocketing" 2 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Talent, Illicit]
  }

fortyOneDerringer :: PlayerCardDef
fortyOneDerringer = (asset "01047" ".41 Derringer" 3 Rogue)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Firearm, Illicit]
  }

leoDeLuca :: PlayerCardDef
leoDeLuca = (asset "01048" "Leo De Luca" 6 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Ally, Criminal]
  }

hardKnocks :: PlayerCardDef
hardKnocks = (asset "01049" "Hard Knocks" 2 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Talent]
  }

elusive :: PlayerCardDef
elusive = (event "01050" "Elusive" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

backstab :: PlayerCardDef
backstab = (event "01051" "Backstab" 3 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcAction = Just Action.Fight
  }

sneakAttack :: PlayerCardDef
sneakAttack = (event "01052" "Sneak Attack" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Tactic]
  }

opportunist :: PlayerCardDef
opportunist = (skill "01053" "Opportunist" [SkillWild] Rogue)
  { pcTraits = setFromList [Innate]
  , pcCommitRestrictions = [OnlyYourTest]
  }

leoDeLuca1 :: PlayerCardDef
leoDeLuca1 = (asset "01054" "Leo De Luca" 5 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Ally, Criminal]
  , pcLevel = 1
  }

catBurgler1 :: PlayerCardDef
catBurgler1 = (asset "01055" "Cat Burgler" 4 Rogue)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Ally, Criminal]
  , pcLevel = 1
  }

sureGamble3 :: PlayerCardDef
sureGamble3 = (asset "01056" "Sure Gamble" 2 Rogue)
  { pcTraits = setFromList [Fortune, Insight]
  , pcFast = True
  , pcWindows = mempty -- We handle this via behavior
  , pcLevel = 3
  }

hotStreak4 :: PlayerCardDef
hotStreak4 = (event "01057" "Hot Streak" 2 Rogue)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Fortune]
  , pcLevel = 4
  }

forbiddenKnowledge :: PlayerCardDef
forbiddenKnowledge =
  (asset "01058" "Forbidden Knowledge" 0 Mystic)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Talent]
    }

holyRosary :: PlayerCardDef
holyRosary = (asset "01059" "Holy Rosary" 2 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Charm]
  }

shrivelling :: PlayerCardDef
shrivelling = (asset "01060" "Shrivelling" 3 Mystic)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Spell]
  }

scrying :: PlayerCardDef
scrying = (asset "01061" "Scrying" 1 Mystic)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Spell]
  }

arcaneStudies :: PlayerCardDef
arcaneStudies = (asset "01062" "Arcane Studies" 2 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Talent]
  }

arcaneInitiate :: PlayerCardDef
arcaneInitiate = (asset "01063" "Arcane Initiate" 1 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally, Sorcerer]
  }

drawnToTheFlame :: PlayerCardDef
drawnToTheFlame = (event "01064" "Drawn to the Flame" 0 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Insight]
  }

wardOfProtection :: PlayerCardDef
wardOfProtection = (event "01065" "Ward of Protection" 1 Mystic)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Spell, Spirit]
  , pcFast = True
  , pcWindows = setFromList [WhenDrawTreachery You]
  }

blindingLight :: PlayerCardDef
blindingLight = (event "01066" "Blinding Light" 2 Mystic)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Spell]
  , pcAction = Just Action.Evade
  }

fearless :: PlayerCardDef
fearless = (skill "01067" "Fearless" [SkillWillpower] Mystic)
  { pcTraits = setFromList [Innate]
  }

mindWipe1 :: PlayerCardDef
mindWipe1 = (event "01068" "Mind Wipe" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Spell]
  , pcLevel = 1
  , pcFast = True
  , pcWindows = setFromList [AnyPhaseBegins]
  }

blindingLight2 :: PlayerCardDef
blindingLight2 = (event "01069" "Blinding Light" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Spell]
  , pcAction = Just Action.Evade
  , pcLevel = 2
  }

bookOfShadows3 :: PlayerCardDef
bookOfShadows3 = (asset "01070" "Book of Shadows" 4 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Item, Tome]
  , pcLevel = 3
  }

grotesqueStatue4 :: PlayerCardDef
grotesqueStatue4 = (asset "01071" "Grotesque Statue" 2 Mystic)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Relic]
  , pcLevel = 4
  }

leatherCoat :: PlayerCardDef
leatherCoat = (asset "01072" "Leather Coat" 0 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Armor]
  }

scavenging :: PlayerCardDef
scavenging = (asset "01073" "Scavenging" 1 Survivor)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Talent]
  }

baseballBat :: PlayerCardDef
baseballBat = (asset "01074" "Baseball Bat" 2 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

rabbitsFoot :: PlayerCardDef
rabbitsFoot = (asset "01075" "Rabbit's Foot" 1 Survivor)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Charm]
  }

strayCat :: PlayerCardDef
strayCat = (asset "01076" "Stray Cat" 1 Survivor)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Ally, Creature]
  }

digDeep :: PlayerCardDef
digDeep = (asset "01077" "Dig Deep" 2 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Talent]
  }

cunningDistraction :: PlayerCardDef
cunningDistraction =
  (event "01078" "Cunning Distraction" 5 Survivor)
    { pcSkills = [SkillIntellect, SkillWild]
    , pcTraits = setFromList [Tactic]
    , pcAction = Just Action.Evade
    }

lookWhatIFound :: PlayerCardDef
lookWhatIFound =
  (event "01079" "\"Look what I found!\"" 2 Survivor)
    { pcSkills = [SkillIntellect, SkillIntellect]
    , pcTraits = setFromList [Fortune]
    , pcFast = True
    , pcWindows = setFromList
      [ AfterFailInvestigationSkillTest You n | n <- [0 .. 2] ]
    }

lucky :: PlayerCardDef
lucky = (event "01080" "Lucky!" 1 Survivor)
  { pcTraits = setFromList [Fortune]
  , pcFast = True
  , pcWindows = setFromList [WhenWouldFailSkillTest You]
  }

survivalInstinct :: PlayerCardDef
survivalInstinct =
  (skill "01081" "Survival Instrinct" [SkillAgility] Survivor)
    { pcTraits = setFromList [Innate]
    }

aquinnah1 :: PlayerCardDef
aquinnah1 = (asset "01082" "Aquinnah" 5 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally]
  , pcLevel = 1
  }

closeCall2 :: PlayerCardDef
closeCall2 = (event "01083" "Close Call" 2 Survivor)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Fortune]
  , pcFast = True
  , pcWindows = mempty -- We handle this via behavior
  , pcLevel = 2
  }

lucky2 :: PlayerCardDef
lucky2 = (event "01084" "Lucky!" 1 Survivor)
  { pcTraits = setFromList [Fortune]
  , pcFast = True
  , pcWindows = setFromList [WhenWouldFailSkillTest You]
  , pcLevel = 2
  }

willToSurvive4 :: PlayerCardDef
willToSurvive4 = (event "01085" "Will to Survive" 4 Survivor)
  { pcSkills = [SkillCombat, SkillWild]
  , pcTraits = setFromList [Spirit]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcLevel = 4
  }

knife :: PlayerCardDef
knife = (asset "01086" "Knife" 1 Neutral)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

flashlight :: PlayerCardDef
flashlight = (asset "01087" "Flashlight" 2 Neutral)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tool]
  }

emergencyCache :: PlayerCardDef
emergencyCache = (event "01088" "Emergency Cache" 0 Neutral)
  { pcTraits = setFromList [Supply]
  }

guts :: PlayerCardDef
guts =
  (skill "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

perception :: PlayerCardDef
perception =
  (skill "01090" "Perceptions" [SkillIntellect, SkillIntellect] Neutral)
    { pcTraits = setFromList [Practiced]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

overpower :: PlayerCardDef
overpower =
  (skill "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
    { pcTraits = setFromList [Practiced]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

manualDexterity :: PlayerCardDef
manualDexterity =
  (skill "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

unexpectedCourage :: PlayerCardDef
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

bulletproofVest3 :: PlayerCardDef
bulletproofVest3 = (asset "01094" "Bulletproof Vest" 2 Neutral)
  { pcSkills = [SkillCombat, SkillWild]
  , pcTraits = setFromList [Item, Armor]
  , pcLevel = 3
  }

elderSignAmulet3 :: PlayerCardDef
elderSignAmulet3 = (asset "01095" "Elder Sign Amulet" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillWild]
  , pcTraits = setFromList [Item, Relic]
  , pcLevel = 3
  }

amnesia :: PlayerCardDef
amnesia = (treachery "01096" "Amnesia" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

paranoia :: PlayerCardDef
paranoia = (treachery "01097" "Paranoia" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

haunted :: PlayerCardDef
haunted = (treachery "01098" "Haunted" 0)
  { pcTraits = setFromList [Curse]
  , pcRevelation = True
  }

psychosis :: PlayerCardDef
psychosis = (treachery "01099" "Psychosis" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

hypochondria :: PlayerCardDef
hypochondria = (treachery "01100" "Hypochondria" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

mobEnforcer :: PlayerCardDef
mobEnforcer = (enemy "01101" "Mob Enforcer" 0)
  { pcTraits = setFromList [Humanoid, Criminal]
  , pcKeywords = setFromList [Keyword.Hunter]
  }

silverTwilightAcolyte :: PlayerCardDef
silverTwilightAcolyte =
  (enemy "01102" "Silver Twilight Acolyte" 0)
    { pcTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , pcKeywords = setFromList [Keyword.Hunter]
    }

stubbornDetective :: PlayerCardDef
stubbornDetective = (enemy "01103" "Stubborn Detective" 0)
  { pcTraits = setFromList [Humanoid, Detective]
  , pcKeywords = setFromList [Keyword.Hunter]
  }

zoeysCross :: PlayerCardDef
zoeysCross = (asset "02006" "Zoey's Cross" 1 Neutral)
  { pcSkills = [SkillCombat, SkillCombat, SkillWild]
  , pcTraits = setFromList [Item, Charm]
  }

smiteTheWicked :: PlayerCardDef
smiteTheWicked = (treachery "02007" "Smite the Wicked" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

searchForTheTruth :: PlayerCardDef
searchForTheTruth =
  (event "02008" "Search for the Truth" 1 Neutral)
    { pcSkills = [SkillIntellect, SkillIntellect, SkillWild]
    , pcTraits = setFromList [Insight]
    }

rexsCurse :: PlayerCardDef
rexsCurse = (treachery "02009" "Rex's Curse" 0)
  { pcTraits = setFromList [Curse]
  , pcRevelation = True
  }

jennysTwin45s :: PlayerCardDef
jennysTwin45s = (asset "02010" "Jenny's Twin .45s" 0 Neutral)
  { pcSkills = [SkillAgility, SkillAgility, SkillWild]
  , pcTraits = setFromList [Item, Weapon, Firearm]
  , pcCost = DynamicCost
  }

searchingForIzzie :: PlayerCardDef
searchingForIzzie = (treachery "02011" "Searching for Izzie" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

jimsTrumpet :: PlayerCardDef
jimsTrumpet = (asset "02012" "Jim's Trumpet" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillWillpower, SkillWild]
  , pcTraits = setFromList [Item, Instrument, Relic]
  }

finalRhapsody :: PlayerCardDef
finalRhapsody = (treachery "02013" "Final Rhapsody" 0)
  { pcTraits = setFromList [Endtimes]
  , pcRevelation = True
  }

duke :: PlayerCardDef
duke = (asset "02014" "Duke" 2 Neutral)
  { pcTraits = setFromList [Ally, Creature]
  }

wrackedByNightmares :: PlayerCardDef
wrackedByNightmares =
  (treachery "02015" "Wracked by Nightmares" 0)
    { pcTraits = setFromList [Madness]
    , pcRevelation = True
    }

blackjack :: PlayerCardDef
blackjack = (asset "02016" "Blackjack" 1 Guardian)
  { pcTraits = setFromList [Item, Weapon, Melee]
  , pcSkills = [SkillCombat]
  }

taunt :: PlayerCardDef
taunt = (event "02017" "Taunt" 1 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcSkills = [SkillWillpower, SkillCombat]
  }

teamwork :: PlayerCardDef
teamwork = (event "02018" "Teamwork" 0 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcSkills = [SkillWild]
  }

taunt2 :: PlayerCardDef
taunt2 = (event "02019" "Taunt" 1 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcSkills = [SkillWillpower, SkillCombat, SkillAgility]
  }

laboratoryAssistant :: PlayerCardDef
laboratoryAssistant =
  (asset "02020" "Laboratory Assistant" 2 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Ally, Miskatonic, Science]
    }

strangeSolution :: PlayerCardDef
strangeSolution = (asset "02021" "Strange Solution" 1 Seeker)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Science]
  }

shortcut :: PlayerCardDef
shortcut = (event "02022" "Shortcut" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Insight, Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

seekingAnswers :: PlayerCardDef
seekingAnswers = (event "02023" "Seeking Answers" 1 Seeker)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = singleton Insight
  }

liquidCourage :: PlayerCardDef
liquidCourage = (asset "02024" "Liquid Courage" 1 Rogue)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Illicit]
  }

thinkOnYourFeet :: PlayerCardDef
thinkOnYourFeet = (event "02025" "Think on Your Feet" 1 Rogue)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = singleton Trick
  , pcFast = True
  , pcWindows = setFromList [WhenEnemySpawns YourLocation []]
  }

doubleOrNothing :: PlayerCardDef
doubleOrNothing =
  (skill "02026" "Double or Nothing" [SkillWild] Rogue)
    { pcTraits = singleton Fortune
    , pcCommitRestrictions = [MaxOnePerTest]
    }

hiredMuscle1 :: PlayerCardDef
hiredMuscle1 = (asset "02027" "Hired Muscle" 1 Rogue)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Ally, Criminal]
  , pcLevel = 1
  }

riteOfSeeking :: PlayerCardDef
riteOfSeeking = (asset "02028" "Rite of Seeking" 4 Mystic)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Spell]
  }

ritualCandles :: PlayerCardDef
ritualCandles = (asset "02029" "Ritual Candles" 1 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = singleton Item
  }

clarityOfMind :: PlayerCardDef
clarityOfMind = (asset "02030" "Clarity of Mind" 2 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = singleton Spell
  }

bindMonster :: PlayerCardDef
bindMonster = (event "02031" "Bind Monster" 3 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = singleton Spell
  , pcAction = Just Action.Evade
  , pcLevel = 2
  }

fireAxe :: PlayerCardDef
fireAxe = (asset "02032" "Fire Axe" 1 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

peterSylvestre :: PlayerCardDef
peterSylvestre = (asset "02033" "Peter Sylvestre" 3 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

baitAndSwitch :: PlayerCardDef
baitAndSwitch = (event "02034" "Bait and Switch" 1 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Trick]
  , pcAction = Just Action.Evade
  }

peterSylvestre2 :: PlayerCardDef
peterSylvestre2 = (asset "02035" "Peter Sylvestre" 3 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally, Miskatonic]
  , pcLevel = 2
  }

kukri :: PlayerCardDef
kukri = (asset "02036" "Kukri" 2 Neutral)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

indebted :: PlayerCardDef
indebted = (treachery "02037" "Indebted" 0)
  { pcTraits = singleton Flaw
  , pcRevelation = True
  , pcPermanent = True
  }

internalInjury :: PlayerCardDef
internalInjury = (treachery "02038" "Internal Injury" 0)
  { pcTraits = singleton Injury
  , pcRevelation = True
  }

chronophobia :: PlayerCardDef
chronophobia = (treachery "02039" "Chronophobia" 0)
  { pcTraits = singleton Madness
  , pcRevelation = True
  }

drHenryArmitage :: PlayerCardDef
drHenryArmitage = (asset "02040" "Dr. Henry Armitage" 2 Neutral)
  { pcSkills = [SkillWild, SkillWild]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

alchemicalConcoction :: PlayerCardDef
alchemicalConcoction =
  (asset "02059" "Alchemical Concoction" 0 Neutral)
    { pcTraits = setFromList [Item, Science]
    }

jazzMulligan :: PlayerCardDef
jazzMulligan = (asset "02060" "\"Jazz\" Mulligan" 0 Neutral)
  { pcTraits = setFromList [Ally, Miskatonic]
  }

professorWarrenRice :: PlayerCardDef
professorWarrenRice =
  (asset "02061" "Progressor Warren Rice" 3 Neutral)
    { pcSkills = [SkillIntellect, SkillWild]
    , pcTraits = setFromList [Ally, Miskatonic]
    }

peterClover :: PlayerCardDef
peterClover = (asset "02079" "Peter Clover" 0 Neutral)
  { pcTraits = setFromList [Humanoid, Criminal]
  }

drFrancisMorgan :: PlayerCardDef
drFrancisMorgan = (asset "02080" "Dr. Francis Morgan" 3 Neutral)
  { pcSkills = [SkillCombat, SkillWild]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

emergencyAid :: PlayerCardDef
emergencyAid = (event "02105" "Emergency Aid" 2 Guardian)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Insight, Science]
  }

brotherXavier1 :: PlayerCardDef
brotherXavier1 = (asset "02106" "Brother Xavier" 5 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally]
  , pcLevel = 1
  }

iveGotAPlan :: PlayerCardDef
iveGotAPlan = (event "02107" "\"I've Got a Plan!\"" 3 Seeker)
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Insight, Tactic]
  }

pathfinder1 :: PlayerCardDef
pathfinder1 = (asset "02108" "Pathfinder" 3 Seeker)
  { pcSkills = [SkillAgility]
  , pcTraits = singleton Talent
  , pcLevel = 1
  }

contraband :: PlayerCardDef
contraband = (event "02109" "Contraband" 4 Rogue)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Supply, Illicit]
  }

adaptable1 :: PlayerCardDef
adaptable1 = (asset "02110" "Adaptable" 0 Rogue)
  { pcPermanent = True
  , pcTraits = setFromList [Talent]
  }

delveTooDeep :: PlayerCardDef
delveTooDeep = (event "02111" "Delve Too Deep" 1 Mystic)
  { pcTraits = setFromList [Insight]
  , pcVictoryPoints = Just 1
  }

songOfTheDead2 :: PlayerCardDef
songOfTheDead2 = (asset "92112" "Song of the Dead" 2 Mystic)
  { pcTraits = setFromList [Spell, Song]
  , pcSkills = [SkillWillpower]
  }

adamLynch :: PlayerCardDef
adamLynch = (asset "02139" "Adam Lynch" 0 Neutral)
  { pcTraits = setFromList [Ally, Miskatonic]
  }

keenEye :: PlayerCardDef
keenEye = (asset "02185" "Keen Eye" 2 Guardian)
  { pcTraits = setFromList [Talent]
  , pcSkills = [SkillIntellect, SkillCombat]
  }

theNecronomiconOlausWormiusTranslation :: PlayerCardDef
theNecronomiconOlausWormiusTranslation =
  (asset "02140" "The Necronomicon" 2 Neutral)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Item, Tome]
    }

bandolier :: PlayerCardDef
bandolier = (asset "02147" "Bandolier" 2 Guardian)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = setFromList [Item]
  }

acrossSpaceAndTime :: PlayerCardDef
acrossSpaceAndTime = (treachery "02178" "Across Space and Time" 0
                            )
  { pcTraits = singleton Madness
  , pcRevelation = True
  }

helplessPassenger :: PlayerCardDef
helplessPassenger = (asset "02179" "Helpless Passenger" 0 Neutral
                           )
  { pcTraits = setFromList [Ally, Bystander]
  , pcKeywords = singleton Keyword.Surge
  }

keenEye3 :: PlayerCardDef
keenEye3 = (asset "02185" "Keen Eye" 0 Guardian)
  { pcTraits = setFromList [Talent]
  , pcPermanent = True
  , pcLevel = 3
  }

keyToTheChamber :: PlayerCardDef
keyToTheChamber = (asset "02215" "Key to the Chamber" 0 Neutral)
  { pcTraits = setFromList [Item, Key]
  }

zebulonWhateley :: PlayerCardDef
zebulonWhateley = (asset "02217" "Zebulon Whateley" 3 Neutral)
  { pcTraits = setFromList [Ally, Dunwich]
  , pcSkills = [SkillWillpower, SkillWild]
  }

earlSawyer :: PlayerCardDef
earlSawyer = (asset "02218" "Earl Sawyer" 3 Neutral)
  { pcTraits = setFromList [Ally, Dunwich]
  , pcSkills = [SkillAgility, SkillWild]
  }

powderOfIbnGhazi :: PlayerCardDef
powderOfIbnGhazi = (asset "02219" "Powder of Ibn-Ghazi" 0 Neutral
                          )
  { pcTraits = singleton Item
  }

springfieldM19034 :: PlayerCardDef
springfieldM19034 = (asset "02226" "Springfiled M1903" 4 Guardian
                           )
  { pcTraits = setFromList [Item, Weapon, Firearm]
  , pcLevel = 4
  , pcSkills = [SkillCombat, SkillAgility]
  }

esotericFormula :: PlayerCardDef
esotericFormula = (asset "02254" "Esoteric Formula" 0 Neutral)
  { pcTraits = singleton Spell
  }

lightningGun5 :: PlayerCardDef
lightningGun5 = (asset "02301" "Lightning Gun" 6 Guardian)
  { pcTraits = setFromList [Item, Weapon, Firearm]
  , pcLevel = 5
  , pcSkills = [SkillIntellect, SkillCombat]
  }

letMeHandleThis :: PlayerCardDef
letMeHandleThis =
  (event "03022" "\"Let me handle this!\"" 0 Guardian)
    { pcSkills = [SkillWillpower, SkillCombat]
    , pcTraits = setFromList [Spirit]
    , pcFast = True
    , pcWindows = mempty -- We handle this via behavior
    }

toothOfEztli :: PlayerCardDef
toothOfEztli = (asset "04023" "Tooth of Eztli" 3 Seeker)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Relic]
  }

secondWind :: PlayerCardDef
secondWind = (event "04149" "Second Wind" 1 Guardian)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Spirit, Bold]
  , pcFast = True -- not fast
  , pcWindows = mempty -- handle via behavior since must be first action
  }

trueUnderstanding :: PlayerCardDef
trueUnderstanding =
  (skill "04153" "True Understanding" [SkillWild] Seeker)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [ScenarioAbility]
    }

occultLexicon :: PlayerCardDef
occultLexicon = (asset "05316" "Occult Lexicon" 2 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tome, Occult]
  }

bloodRite :: PlayerCardDef
bloodRite = (event "05317" "Blood Rite" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Spell]
  }

firstWatch :: PlayerCardDef
firstWatch = (event "06110" "First Watch" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [WhenAllDrawEncounterCard]
  }

scrollOfProphecies :: PlayerCardDef
scrollOfProphecies =
  (asset "06116" "Scroll of Prophecies" 3 Mystic)
    { pcSkills = [SkillWillpower]
    , pcTraits = setFromList [Item, Tome]
    }

astoundingRevelation :: PlayerCardDef
astoundingRevelation =
  (event "06023" "Astounding Revelation" 0 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Research]
    , pcFast = True
    , pcWindows = mempty -- cannot be played
    }

litaChantler :: PlayerCardDef
litaChantler = (asset "01117" "Lita Chantler" 0 Neutral)
  { pcTraits = setFromList [Ally]
  }

physicalTraining2 :: PlayerCardDef
physicalTraining2 = (asset "50001" "Physical Training" 0 Guardian
                           )
  { pcSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillCombat]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

dynamiteBlast2 :: PlayerCardDef
dynamiteBlast2 = (event "50002" "Dynamite Blast" 4 Guardian)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Tactic]
  , pcAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , pcLevel = 2
  }

hyperawareness2 :: PlayerCardDef
hyperawareness2 = (asset "50003" "Hyperawareness" 0 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect, SkillAgility, SkillAgility]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

barricade3 :: PlayerCardDef
barricade3 = (event "50004" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Insight, Tactic]
  , pcLevel = 3
  }

hardKnocks2 :: PlayerCardDef
hardKnocks2 = (asset "50005" "Hard Knocks" 0 Rogue)
  { pcSkills = [SkillCombat, SkillCombat, SkillAgility, SkillAgility]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

hotStreak2 :: PlayerCardDef
hotStreak2 = (event "50006" "Hot Streak" 5 Rogue)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Fortune]
  , pcLevel = 2
  }

arcaneStudies2 :: PlayerCardDef
arcaneStudies2 = (asset "50007" "Arcane Studies" 0 Mystic)
  { pcSkills = [SkillWillpower, SkillWillpower, SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

mindWipe3 :: PlayerCardDef
mindWipe3 = (event "50008" "Mind Wipe" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Spell]
  , pcLevel = 3
  , pcFast = True
  , pcWindows = setFromList [AnyPhaseBegins]
  }

digDeep2 :: PlayerCardDef
digDeep2 = (asset "50009" "Dig Deep" 0 Survivor)
  { pcSkills = [SkillWillpower, SkillWillpower, SkillAgility, SkillAgility]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

rabbitsFoot3 :: PlayerCardDef
rabbitsFoot3 = (asset "50010" "Rabbit's Foot" 1 Survivor)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Charm]
  , pcLevel = 3
  }

contraband2 :: PlayerCardDef
contraband2 = (event "51005" "Contraband" 3 Rogue)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Supply, Illicit]
  , pcLevel = 2
  }

taunt3 :: PlayerCardDef
taunt3 = (event "60130" "Taunt" 1 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [FastPlayerWindow]
  , pcSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillAgility]
  }

arcaneEnlightenment :: PlayerCardDef
arcaneEnlightenment =
  (asset "60205" "Arcane Enlightenment" 2 Seeker)
    { pcSkills = [SkillWillpower, SkillWillpower]
    , pcTraits = setFromList [Ritual]
    }

celaenoFragments :: PlayerCardDef
celaenoFragments = (asset "60206" "Celaeno Fragments" 1 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tome]
  }

encyclopedia :: PlayerCardDef
encyclopedia = (asset "60208" "Encyclopedia" 2 Seeker)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Tome]
  }

higherEducation :: PlayerCardDef
higherEducation = (asset "60211" "Higher Education" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Talent]
  }

whittonGreene :: PlayerCardDef
whittonGreene = (asset "60213" "Whitton Greene" 4 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

iveGotAPlan2 :: PlayerCardDef
iveGotAPlan2 = (event "60225" "\"I've Got a Plan!\"" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Insight, Tactic]
  }

atychiphobia :: PlayerCardDef
atychiphobia = (treachery "60504" "Atychiphobia" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

ladyEsprit :: PlayerCardDef
ladyEsprit = (asset "81019" "Lady Espirt" 4 Neutral)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = setFromList [Ally, Sorcerer]
  }

bearTrap :: PlayerCardDef
bearTrap =
  (asset "81020" "Bear Trap" 0 Neutral) { pcTraits = setFromList [Trap] }

fishingNet :: PlayerCardDef
fishingNet = (asset "81021" "Fishing Net" 0 Neutral)
  { pcTraits = setFromList [Trap]
  }

curseOfTheRougarou :: PlayerCardDef
curseOfTheRougarou = (treachery "81029" "Curse of the Rougarou" 0
                            )
  { pcTraits = setFromList [Curse]
  , pcRevelation = True
  }

monstrousTransformation :: PlayerCardDef
monstrousTransformation =
  (asset "81030" "Monstrous Transformation" 0 Neutral)
    { pcTraits = setFromList [Talent]
    , pcFast = True
    , pcWindows = setFromList [DuringTurn You]
    }

daisysToteBagAdvanced :: PlayerCardDef
daisysToteBagAdvanced =
  (asset "90002" "Daisy's Tote Bag" 2 Neutral)
    { pcSkills = [SkillWillpower, SkillIntellect, SkillWild, SkillWild]
    , pcTraits = setFromList [Item]
    }

theNecronomiconAdvanced :: PlayerCardDef
theNecronomiconAdvanced =
  (asset "90003" "The Necronomicon" 0 Neutral)
    { pcTraits = setFromList [Item, Tome]
    , pcWeakness = True
    , pcRevelation = True
    }
