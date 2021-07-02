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

import Arkham.Event.Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
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
genPlayerCard cardCode = lookupPlayerCard cardCode <$> getRandom

lookupPlayerCardName :: CardCode -> Name
lookupPlayerCardName = cdName . lookupPlayerCardDef

lookupPlayerCard :: CardCode -> CardId -> PlayerCard
lookupPlayerCard cardCode cardId = MkPlayerCard
  { pcId = cardId
  , pcDef = lookupPlayerCardDef cardCode
  , pcBearer = Nothing
  }

lookupPlayerCardDef :: CardCode -> CardDef
lookupPlayerCardDef cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

allPlayerCards :: HashMap CardCode CardDef
allPlayerCards = allEventCards <> mapFromList
  [ ("asset", placeholderAsset)
  , ("01006", rolands38Special)
  , ("01007", coverUp)
  , ("01008", daisysToteBag)
  , ("01009", theNecronomicon)
  , ("01011", hospitalDebts)
  , ("01012", heirloomOfHyperborea)
  , ("01014", wendysAmulet)
  , ("01015", abandonedAndAlone)
  , ("01016", fortyFiveAutomatic)
  , ("01017", physicalTraining)
  , ("01018", beatCop)
  , ("01019", firstAid)
  , ("01020", machete)
  , ("01021", guardDog)
  , ("01025", viciousBlow)
  , ("01027", policeBadge2)
  , ("01028", beatCop2)
  , ("01029", shotgun4)
  , ("01030", magnifyingGlass)
  , ("01031", oldBookOfLore)
  , ("01032", researchLibrarian)
  , ("01033", drMilanChristopher)
  , ("01034", hyperawareness)
  , ("01035", medicalTexts)
  , ("01039", deduction)
  , ("01040", magnifyingGlass1)
  , ("01041", discOfItzamna2)
  , ("01042", encyclopedia2)
  , ("01044", switchblade)
  , ("01045", burglary)
  , ("01046", pickpoketing)
  , ("01047", fortyOneDerringer)
  , ("01048", leoDeLuca)
  , ("01049", hardKnocks)
  , ("01053", opportunist)
  , ("01054", leoDeLuca1)
  , ("01055", catBurgler1)
  , ("01056", sureGamble3)
  , ("01058", forbiddenKnowledge)
  , ("01059", holyRosary)
  , ("01060", shrivelling)
  , ("01061", scrying)
  , ("01062", arcaneStudies)
  , ("01063", arcaneInitiate)
  , ("01067", fearless)
  , ("01070", bookOfShadows3)
  , ("01071", grotesqueStatue4)
  , ("01072", leatherCoat)
  , ("01073", scavenging)
  , ("01074", baseballBat)
  , ("01075", rabbitsFoot)
  , ("01076", strayCat)
  , ("01077", digDeep)
  , ("01081", survivalInstinct)
  , ("01082", aquinnah1)
  , ("01086", knife)
  , ("01087", flashlight)
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
  , ("02009", rexsCurse)
  , ("02010", jennysTwin45s)
  , ("02011", searchingForIzzie)
  , ("02012", jimsTrumpet)
  , ("02013", finalRhapsody)
  , ("02014", duke)
  , ("02015", wrackedByNightmares)
  , ("02016", blackjack)
  , ("02020", laboratoryAssistant)
  , ("02021", strangeSolution)
  , ("02024", liquidCourage)
  , ("02026", doubleOrNothing)
  , ("02027", hiredMuscle1)
  , ("02028", riteOfSeeking)
  , ("02029", ritualCandles)
  , ("02030", clarityOfMind)
  , ("02032", fireAxe)
  , ("02033", peterSylvestre)
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
  , ("02106", brotherXavier1)
  , ("02108", pathfinder1)
  , ("02110", adaptable1)
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
  , ("04023", toothOfEztli)
  , ("04153", trueUnderstanding)
  , ("05316", occultLexicon)
  , ("06116", scrollOfProphecies)
  , ("07152", keenEye)
  , ("50001", physicalTraining2)
  , ("50003", hyperawareness2)
  , ("50005", hardKnocks2)
  , ("50007", arcaneStudies2)
  , ("50009", digDeep2)
  , ("50010", rabbitsFoot3)
  , ("60205", arcaneEnlightenment)
  , ("60206", celaenoFragments)
  , ("60208", encyclopedia)
  , ("60211", higherEducation)
  , ("60213", whittonGreene)
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
  -> Name
  -> Int
  -> CardType
  -> ClassSymbol
  -> CardDef
basePlayerCard cardCode name cost cardType classSymbol = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Just (StaticCost cost)
  , cdLevel = 0
  , cdCardType = cardType
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

asset :: CardCode -> Name -> Int -> ClassSymbol -> CardDef
asset cardCode name cost classSymbol =
  basePlayerCard cardCode name cost AssetType classSymbol

skill :: CardCode -> Name -> [SkillType] -> ClassSymbol -> CardDef
skill cardCode name skills classSymbol =
  (basePlayerCard cardCode name 0 SkillType classSymbol)
    { cdSkills = skills
    }

treachery :: CardCode -> Name -> Int -> CardDef
treachery cardCode name cost =
  (basePlayerCard cardCode name cost PlayerTreacheryType Neutral)
    { cdWeakness = True
    }

enemy :: CardCode -> Name -> Int -> CardDef
enemy cardCode name cost =
  (basePlayerCard cardCode name cost PlayerEnemyType Neutral)
    { cdWeakness = True
    }

placeholderAsset :: CardDef
placeholderAsset = asset "asset" "Placeholder Asset" 0 Neutral

rolands38Special :: CardDef
rolands38Special =
  (asset "01006" "Roland's .38 Special" 3 Neutral)
    { cdSkills = [SkillCombat, SkillAgility, SkillWild]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    }

coverUp :: CardDef
coverUp = (treachery "01007" "Cover Up" 0)
  { cdCardTraits = setFromList [Task]
  , cdRevelation = True
  }

daisysToteBag :: CardDef
daisysToteBag = (asset "01008" "Daisy's Tote Bag" 2 Neutral)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , cdCardTraits = setFromList [Item]
  }

theNecronomicon :: CardDef
theNecronomicon = (asset "01009" "The Necronomicon" 0 Neutral)
  { cdCardTraits = setFromList [Item, Tome]
  , cdWeakness = True
  , cdRevelation = True
  }

hospitalDebts :: CardDef
hospitalDebts = (treachery "01011" "Hospital Debts" 0)
  { cdCardTraits = setFromList [Task]
  , cdRevelation = True
  }

heirloomOfHyperborea :: CardDef
heirloomOfHyperborea =
  (asset "01012" "Heirloom of Hyperborea" 3 Neutral)
    { cdSkills = [SkillWillpower, SkillCombat, SkillWild]
    , cdCardTraits = setFromList [Item, Relic]
    }

wendysAmulet :: CardDef
wendysAmulet = (asset "01014" "Wendy's Amulet" 2 Neutral)
  { cdSkills = [SkillWild, SkillWild]
  , cdCardTraits = setFromList [Item, Relic]
  }

abandonedAndAlone :: CardDef
abandonedAndAlone = (treachery "01015" "Abandoned and Alone" 0)
  { cdCardTraits = setFromList [Madness]
  , cdRevelation = True
  }

fortyFiveAutomatic :: CardDef
fortyFiveAutomatic = (asset "01016" ".45 Automatic" 4 Guardian)
  { cdSkills = [SkillAgility]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  }

physicalTraining :: CardDef
physicalTraining = (asset "01017" "Physical Training" 2 Guardian)
  { cdSkills = [SkillWillpower, SkillCombat]
  , cdCardTraits = setFromList [Talent]
  }

beatCop :: CardDef
beatCop = (asset "01018" "Beat Cop" 4 Guardian)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Ally, Police]
  }

firstAid :: CardDef
firstAid = (asset "01019" "First Aid" 2 Guardian)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Talent, Science]
  }

machete :: CardDef
machete = (asset "01020" "Machete" 3 Guardian)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  }

guardDog :: CardDef
guardDog = (asset "01021" "Guard Dog" 3 Guardian)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Ally, Creature]
  }

viciousBlow :: CardDef
viciousBlow =
  (skill "01025" "Vicious Blow" [SkillCombat] Guardian)
    { cdCardTraits = setFromList [Practiced]
    }

policeBadge2 :: CardDef
policeBadge2 = (asset "01027" "Police Badge" 3 Guardian)
  { cdSkills = [SkillWillpower, SkillWild]
  , cdCardTraits = setFromList [Item]
  , cdLevel = 2
  }

beatCop2 :: CardDef
beatCop2 = (asset "01028" "Beat Cop" 4 Guardian)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = setFromList [Ally, Police]
  , cdLevel = 2
  }

shotgun4 :: CardDef
shotgun4 = (asset "01029" "Shotgun" 5 Guardian)
  { cdSkills = [SkillCombat, SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdLevel = 4
  }

magnifyingGlass :: CardDef
magnifyingGlass = (asset "01030" "Magnifying Glass" 1 Seeker)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

oldBookOfLore :: CardDef
oldBookOfLore = (asset "01031" "Old Book of Lore" 3 Seeker)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Item, Tome]
  }

researchLibrarian :: CardDef
researchLibrarian = (asset "01032" "Research Librarian" 2 Seeker)
  { cdSkills = [SkillAgility]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  }

drMilanChristopher :: CardDef
drMilanChristopher =
  (asset "01033" "Dr. Milan Christopher" 4 Seeker)
    { cdSkills = [SkillIntellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    }

hyperawareness :: CardDef
hyperawareness = (asset "01034" "Hyperawareness" 2 Seeker)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Talent]
  }

medicalTexts :: CardDef
medicalTexts = (asset "01035" "Medical Texts" 2 Seeker)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Tome]
  }

deduction :: CardDef
deduction = (skill "01039" "Deduction" [SkillIntellect] Seeker)
  { cdCardTraits = setFromList [Practiced]
  }

magnifyingGlass1 :: CardDef
magnifyingGlass1 = (asset "01040" "Magnifying Glass" 0 Seeker)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Item, Tool]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  , cdLevel = 1
  }

discOfItzamna2 :: CardDef
discOfItzamna2 = (asset "01041" "Disc of Itzamna" 3 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , cdCardTraits = setFromList [Item, Relic]
  , cdLevel = 2
  }

encyclopedia2 :: CardDef
encyclopedia2 = (asset "01042" "Encyclopedia" 2 Seeker)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Item, Tome]
  , cdLevel = 2
  }

switchblade :: CardDef
switchblade = (asset "01044" "Switchbalde" 1 Rogue)
  { cdSkills = [SkillAgility]
  , cdCardTraits = setFromList [Item, Weapon, Melee, Illicit]
  , cdFast = True
  , cdWindows = setFromList [DuringTurn You]
  }

burglary :: CardDef
burglary = (asset "01045" "Burglary" 1 Rogue)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Talent, Illicit]
  }

pickpoketing :: CardDef
pickpoketing = (asset "01046" "Pickpocketing" 2 Rogue)
  { cdSkills = [SkillAgility]
  , cdCardTraits = setFromList [Talent, Illicit]
  }

fortyOneDerringer :: CardDef
fortyOneDerringer = (asset "01047" ".41 Derringer" 3 Rogue)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
  }

leoDeLuca :: CardDef
leoDeLuca = (asset "01048" "Leo De Luca" 6 Rogue)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Ally, Criminal]
  }

hardKnocks :: CardDef
hardKnocks = (asset "01049" "Hard Knocks" 2 Rogue)
  { cdSkills = [SkillCombat, SkillAgility]
  , cdCardTraits = setFromList [Talent]
  }

opportunist :: CardDef
opportunist = (skill "01053" "Opportunist" [SkillWild] Rogue)
  { cdCardTraits = setFromList [Innate]
  , cdCommitRestrictions = [OnlyYourTest]
  }

leoDeLuca1 :: CardDef
leoDeLuca1 = (asset "01054" "Leo De Luca" 5 Rogue)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdLevel = 1
  }

catBurgler1 :: CardDef
catBurgler1 = (asset "01055" "Cat Burgler" 4 Rogue)
  { cdSkills = [SkillWillpower, SkillAgility]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdLevel = 1
  }

sureGamble3 :: CardDef
sureGamble3 = (asset "01056" "Sure Gamble" 2 Rogue)
  { cdCardTraits = setFromList [Fortune, Insight]
  , cdFast = True
  , cdWindows = mempty -- We handle this via behavior
  , cdLevel = 3
  }

forbiddenKnowledge :: CardDef
forbiddenKnowledge =
  (asset "01058" "Forbidden Knowledge" 0 Mystic)
    { cdSkills = [SkillIntellect]
    , cdCardTraits = setFromList [Talent]
    }

holyRosary :: CardDef
holyRosary = (asset "01059" "Holy Rosary" 2 Mystic)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Item, Charm]
  }

shrivelling :: CardDef
shrivelling = (asset "01060" "Shrivelling" 3 Mystic)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Spell]
  }

scrying :: CardDef
scrying = (asset "01061" "Scrying" 1 Mystic)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Spell]
  }

arcaneStudies :: CardDef
arcaneStudies = (asset "01062" "Arcane Studies" 2 Mystic)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = setFromList [Talent]
  }

arcaneInitiate :: CardDef
arcaneInitiate = (asset "01063" "Arcane Initiate" 1 Mystic)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Ally, Sorcerer]
  }

fearless :: CardDef
fearless = (skill "01067" "Fearless" [SkillWillpower] Mystic)
  { cdCardTraits = setFromList [Innate]
  }

bookOfShadows3 :: CardDef
bookOfShadows3 = (asset "01070" "Book of Shadows" 4 Mystic)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = setFromList [Item, Tome]
  , cdLevel = 3
  }

grotesqueStatue4 :: CardDef
grotesqueStatue4 = (asset "01071" "Grotesque Statue" 2 Mystic)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdLevel = 4
  }

leatherCoat :: CardDef
leatherCoat = (asset "01072" "Leather Coat" 0 Survivor)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Armor]
  }

scavenging :: CardDef
scavenging = (asset "01073" "Scavenging" 1 Survivor)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Talent]
  }

baseballBat :: CardDef
baseballBat = (asset "01074" "Baseball Bat" 2 Survivor)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  }

rabbitsFoot :: CardDef
rabbitsFoot = (asset "01075" "Rabbit's Foot" 1 Survivor)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Item, Charm]
  }

strayCat :: CardDef
strayCat = (asset "01076" "Stray Cat" 1 Survivor)
  { cdSkills = [SkillAgility]
  , cdCardTraits = setFromList [Ally, Creature]
  }

digDeep :: CardDef
digDeep = (asset "01077" "Dig Deep" 2 Survivor)
  { cdSkills = [SkillIntellect, SkillAgility]
  , cdCardTraits = setFromList [Talent]
  }

survivalInstinct :: CardDef
survivalInstinct =
  (skill "01081" "Survival Instrinct" [SkillAgility] Survivor)
    { cdCardTraits = setFromList [Innate]
    }

aquinnah1 :: CardDef
aquinnah1 = (asset "01082" "Aquinnah" 5 Survivor)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Ally]
  , cdLevel = 1
  }

knife :: CardDef
knife = (asset "01086" "Knife" 1 Neutral)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  }

flashlight :: CardDef
flashlight = (asset "01087" "Flashlight" 2 Neutral)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Item, Tool]
  }

guts :: CardDef
guts =
  (skill "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

perception :: CardDef
perception =
  (skill "01090" "Perceptions" [SkillIntellect, SkillIntellect] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

overpower :: CardDef
overpower =
  (skill "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
    { cdCardTraits = setFromList [Practiced]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

manualDexterity :: CardDef
manualDexterity =
  (skill "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

unexpectedCourage :: CardDef
unexpectedCourage =
  (skill "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

bulletproofVest3 :: CardDef
bulletproofVest3 = (asset "01094" "Bulletproof Vest" 2 Neutral)
  { cdSkills = [SkillCombat, SkillWild]
  , cdCardTraits = setFromList [Item, Armor]
  , cdLevel = 3
  }

elderSignAmulet3 :: CardDef
elderSignAmulet3 = (asset "01095" "Elder Sign Amulet" 2 Neutral)
  { cdSkills = [SkillWillpower, SkillWild]
  , cdCardTraits = setFromList [Item, Relic]
  , cdLevel = 3
  }

amnesia :: CardDef
amnesia = (treachery "01096" "Amnesia" 0)
  { cdCardTraits = setFromList [Madness]
  , cdRevelation = True
  }

paranoia :: CardDef
paranoia = (treachery "01097" "Paranoia" 0)
  { cdCardTraits = setFromList [Madness]
  , cdRevelation = True
  }

haunted :: CardDef
haunted = (treachery "01098" "Haunted" 0)
  { cdCardTraits = setFromList [Curse]
  , cdRevelation = True
  }

psychosis :: CardDef
psychosis = (treachery "01099" "Psychosis" 0)
  { cdCardTraits = setFromList [Madness]
  , cdRevelation = True
  }

hypochondria :: CardDef
hypochondria = (treachery "01100" "Hypochondria" 0)
  { cdCardTraits = setFromList [Madness]
  , cdRevelation = True
  }

mobEnforcer :: CardDef
mobEnforcer = (enemy "01101" "Mob Enforcer" 0)
  { cdCardTraits = setFromList [Humanoid, Criminal]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

silverTwilightAcolyte :: CardDef
silverTwilightAcolyte =
  (enemy "01102" "Silver Twilight Acolyte" 0)
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

stubbornDetective :: CardDef
stubbornDetective = (enemy "01103" "Stubborn Detective" 0)
  { cdCardTraits = setFromList [Humanoid, Detective]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

zoeysCross :: CardDef
zoeysCross = (asset "02006" "Zoey's Cross" 1 Neutral)
  { cdSkills = [SkillCombat, SkillCombat, SkillWild]
  , cdCardTraits = setFromList [Item, Charm]
  }

smiteTheWicked :: CardDef
smiteTheWicked = (treachery "02007" "Smite the Wicked" 0)
  { cdCardTraits = setFromList [Task]
  , cdRevelation = True
  }

rexsCurse :: CardDef
rexsCurse = (treachery "02009" "Rex's Curse" 0)
  { cdCardTraits = setFromList [Curse]
  , cdRevelation = True
  }

jennysTwin45s :: CardDef
jennysTwin45s = (asset "02010" "Jenny's Twin .45s" 0 Neutral)
  { cdSkills = [SkillAgility, SkillAgility, SkillWild]
  , cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdCost = Just DynamicCost
  }

searchingForIzzie :: CardDef
searchingForIzzie = (treachery "02011" "Searching for Izzie" 0)
  { cdCardTraits = setFromList [Task]
  , cdRevelation = True
  }

jimsTrumpet :: CardDef
jimsTrumpet = (asset "02012" "Jim's Trumpet" 2 Neutral)
  { cdSkills = [SkillWillpower, SkillWillpower, SkillWild]
  , cdCardTraits = setFromList [Item, Instrument, Relic]
  }

finalRhapsody :: CardDef
finalRhapsody = (treachery "02013" "Final Rhapsody" 0)
  { cdCardTraits = setFromList [Endtimes]
  , cdRevelation = True
  }

duke :: CardDef
duke = (asset "02014" "Duke" 2 Neutral)
  { cdCardTraits = setFromList [Ally, Creature]
  }

wrackedByNightmares :: CardDef
wrackedByNightmares =
  (treachery "02015" "Wracked by Nightmares" 0)
    { cdCardTraits = setFromList [Madness]
    , cdRevelation = True
    }

blackjack :: CardDef
blackjack = (asset "02016" "Blackjack" 1 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Melee]
  , cdSkills = [SkillCombat]
  }

laboratoryAssistant :: CardDef
laboratoryAssistant =
  (asset "02020" "Laboratory Assistant" 2 Seeker)
    { cdSkills = [SkillIntellect]
    , cdCardTraits = setFromList [Ally, Miskatonic, Science]
    }

strangeSolution :: CardDef
strangeSolution = (asset "02021" "Strange Solution" 1 Seeker)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Item, Science]
  }

liquidCourage :: CardDef
liquidCourage = (asset "02024" "Liquid Courage" 1 Rogue)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Item, Illicit]
  }

doubleOrNothing :: CardDef
doubleOrNothing =
  (skill "02026" "Double or Nothing" [SkillWild] Rogue)
    { cdCardTraits = singleton Fortune
    , cdCommitRestrictions = [MaxOnePerTest]
    }

hiredMuscle1 :: CardDef
hiredMuscle1 = (asset "02027" "Hired Muscle" 1 Rogue)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Ally, Criminal]
  , cdLevel = 1
  }

riteOfSeeking :: CardDef
riteOfSeeking = (asset "02028" "Rite of Seeking" 4 Mystic)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Spell]
  }

ritualCandles :: CardDef
ritualCandles = (asset "02029" "Ritual Candles" 1 Mystic)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = singleton Item
  }

clarityOfMind :: CardDef
clarityOfMind = (asset "02030" "Clarity of Mind" 2 Mystic)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = singleton Spell
  }

fireAxe :: CardDef
fireAxe = (asset "02032" "Fire Axe" 1 Survivor)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  }

peterSylvestre :: CardDef
peterSylvestre = (asset "02033" "Peter Sylvestre" 3 Survivor)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  }

peterSylvestre2 :: CardDef
peterSylvestre2 = (asset "02035" "Peter Sylvestre" 3 Survivor)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  , cdLevel = 2
  }

kukri :: CardDef
kukri = (asset "02036" "Kukri" 2 Neutral)
  { cdSkills = [SkillCombat]
  , cdCardTraits = setFromList [Item, Weapon, Melee]
  }

indebted :: CardDef
indebted = (treachery "02037" "Indebted" 0)
  { cdCardTraits = singleton Flaw
  , cdRevelation = True
  , cdPermanent = True
  }

internalInjury :: CardDef
internalInjury = (treachery "02038" "Internal Injury" 0)
  { cdCardTraits = singleton Injury
  , cdRevelation = True
  }

chronophobia :: CardDef
chronophobia = (treachery "02039" "Chronophobia" 0)
  { cdCardTraits = singleton Madness
  , cdRevelation = True
  }

drHenryArmitage :: CardDef
drHenryArmitage = (asset "02040" "Dr. Henry Armitage" 2 Neutral)
  { cdSkills = [SkillWild, SkillWild]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  }

alchemicalConcoction :: CardDef
alchemicalConcoction =
  (asset "02059" "Alchemical Concoction" 0 Neutral)
    { cdCardTraits = setFromList [Item, Science]
    }

jazzMulligan :: CardDef
jazzMulligan = (asset "02060" "\"Jazz\" Mulligan" 0 Neutral)
  { cdCardTraits = setFromList [Ally, Miskatonic]
  }

professorWarrenRice :: CardDef
professorWarrenRice =
  (asset "02061" "Progressor Warren Rice" 3 Neutral)
    { cdSkills = [SkillIntellect, SkillWild]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    }

peterClover :: CardDef
peterClover = (asset "02079" "Peter Clover" 0 Neutral)
  { cdCardTraits = setFromList [Humanoid, Criminal]
  }

drFrancisMorgan :: CardDef
drFrancisMorgan = (asset "02080" "Dr. Francis Morgan" 3 Neutral)
  { cdSkills = [SkillCombat, SkillWild]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  }

brotherXavier1 :: CardDef
brotherXavier1 = (asset "02106" "Brother Xavier" 5 Guardian)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Ally]
  , cdLevel = 1
  }

pathfinder1 :: CardDef
pathfinder1 = (asset "02108" "Pathfinder" 3 Seeker)
  { cdSkills = [SkillAgility]
  , cdCardTraits = singleton Talent
  , cdLevel = 1
  }

adaptable1 :: CardDef
adaptable1 = (asset "02110" "Adaptable" 0 Rogue)
  { cdPermanent = True
  , cdCardTraits = setFromList [Talent]
  }

songOfTheDead2 :: CardDef
songOfTheDead2 = (asset "92112" "Song of the Dead" 2 Mystic)
  { cdCardTraits = setFromList [Spell, Song]
  , cdSkills = [SkillWillpower]
  }

adamLynch :: CardDef
adamLynch = (asset "02139" "Adam Lynch" 0 Neutral)
  { cdCardTraits = setFromList [Ally, Miskatonic]
  }

keenEye :: CardDef
keenEye = (asset "02185" "Keen Eye" 2 Guardian)
  { cdCardTraits = setFromList [Talent]
  , cdSkills = [SkillIntellect, SkillCombat]
  }

theNecronomiconOlausWormiusTranslation :: CardDef
theNecronomiconOlausWormiusTranslation =
  (asset "02140" "The Necronomicon" 2 Neutral)
    { cdSkills = [SkillIntellect]
    , cdCardTraits = setFromList [Item, Tome]
    }

bandolier :: CardDef
bandolier = (asset "02147" "Bandolier" 2 Guardian)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , cdCardTraits = setFromList [Item]
  }

acrossSpaceAndTime :: CardDef
acrossSpaceAndTime = (treachery "02178" "Across Space and Time" 0
                            )
  { cdCardTraits = singleton Madness
  , cdRevelation = True
  }

helplessPassenger :: CardDef
helplessPassenger = (asset "02179" "Helpless Passenger" 0 Neutral
                           )
  { cdCardTraits = setFromList [Ally, Bystander]
  , cdKeywords = singleton Keyword.Surge
  }

keenEye3 :: CardDef
keenEye3 = (asset "02185" "Keen Eye" 0 Guardian)
  { cdCardTraits = setFromList [Talent]
  , cdPermanent = True
  , cdLevel = 3
  }

keyToTheChamber :: CardDef
keyToTheChamber = (asset "02215" "Key to the Chamber" 0 Neutral)
  { cdCardTraits = setFromList [Item, Key]
  }

zebulonWhateley :: CardDef
zebulonWhateley = (asset "02217" "Zebulon Whateley" 3 Neutral)
  { cdCardTraits = setFromList [Ally, Dunwich]
  , cdSkills = [SkillWillpower, SkillWild]
  }

earlSawyer :: CardDef
earlSawyer = (asset "02218" "Earl Sawyer" 3 Neutral)
  { cdCardTraits = setFromList [Ally, Dunwich]
  , cdSkills = [SkillAgility, SkillWild]
  }

powderOfIbnGhazi :: CardDef
powderOfIbnGhazi = (asset "02219" "Powder of Ibn-Ghazi" 0 Neutral
                          )
  { cdCardTraits = singleton Item
  }

springfieldM19034 :: CardDef
springfieldM19034 = (asset "02226" "Springfiled M1903" 4 Guardian
                           )
  { cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdLevel = 4
  , cdSkills = [SkillCombat, SkillAgility]
  }

esotericFormula :: CardDef
esotericFormula = (asset "02254" "Esoteric Formula" 0 Neutral)
  { cdCardTraits = singleton Spell
  }

lightningGun5 :: CardDef
lightningGun5 = (asset "02301" "Lightning Gun" 6 Guardian)
  { cdCardTraits = setFromList [Item, Weapon, Firearm]
  , cdLevel = 5
  , cdSkills = [SkillIntellect, SkillCombat]
  }

toothOfEztli :: CardDef
toothOfEztli = (asset "04023" "Tooth of Eztli" 3 Seeker)
  { cdSkills = [SkillWillpower]
  , cdCardTraits = setFromList [Item, Relic]
  }

trueUnderstanding :: CardDef
trueUnderstanding =
  (skill "04153" "True Understanding" [SkillWild] Seeker)
    { cdCardTraits = setFromList [Innate]
    , cdCommitRestrictions = [ScenarioAbility]
    }

occultLexicon :: CardDef
occultLexicon = (asset "05316" "Occult Lexicon" 2 Seeker)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Item, Tome, Occult]
  }

scrollOfProphecies :: CardDef
scrollOfProphecies =
  (asset "06116" "Scroll of Prophecies" 3 Mystic)
    { cdSkills = [SkillWillpower]
    , cdCardTraits = setFromList [Item, Tome]
    }

litaChantler :: CardDef
litaChantler = (asset "01117" "Lita Chantler" 0 Neutral)
  { cdCardTraits = setFromList [Ally]
  }

physicalTraining2 :: CardDef
physicalTraining2 = (asset "50001" "Physical Training" 0 Guardian
                           )
  { cdSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillCombat]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

hyperawareness2 :: CardDef
hyperawareness2 = (asset "50003" "Hyperawareness" 0 Seeker)
  { cdSkills = [SkillIntellect, SkillIntellect, SkillAgility, SkillAgility]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

hardKnocks2 :: CardDef
hardKnocks2 = (asset "50005" "Hard Knocks" 0 Rogue)
  { cdSkills = [SkillCombat, SkillCombat, SkillAgility, SkillAgility]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

arcaneStudies2 :: CardDef
arcaneStudies2 = (asset "50007" "Arcane Studies" 0 Mystic)
  { cdSkills = [SkillWillpower, SkillWillpower, SkillIntellect, SkillIntellect]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

digDeep2 :: CardDef
digDeep2 = (asset "50009" "Dig Deep" 0 Survivor)
  { cdSkills = [SkillWillpower, SkillWillpower, SkillAgility, SkillAgility]
  , cdCardTraits = setFromList [Talent]
  , cdLevel = 2
  }

rabbitsFoot3 :: CardDef
rabbitsFoot3 = (asset "50010" "Rabbit's Foot" 1 Survivor)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Item, Charm]
  , cdLevel = 3
  }

arcaneEnlightenment :: CardDef
arcaneEnlightenment =
  (asset "60205" "Arcane Enlightenment" 2 Seeker)
    { cdSkills = [SkillWillpower, SkillWillpower]
    , cdCardTraits = setFromList [Ritual]
    }

celaenoFragments :: CardDef
celaenoFragments = (asset "60206" "Celaeno Fragments" 1 Seeker)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Item, Tome]
  }

encyclopedia :: CardDef
encyclopedia = (asset "60208" "Encyclopedia" 2 Seeker)
  { cdSkills = [SkillWild]
  , cdCardTraits = setFromList [Item, Tome]
  }

higherEducation :: CardDef
higherEducation = (asset "60211" "Higher Education" 0 Seeker)
  { cdSkills = [SkillWillpower, SkillIntellect]
  , cdCardTraits = setFromList [Talent]
  }

whittonGreene :: CardDef
whittonGreene = (asset "60213" "Whitton Greene" 4 Seeker)
  { cdSkills = [SkillIntellect]
  , cdCardTraits = setFromList [Ally, Miskatonic]
  }

atychiphobia :: CardDef
atychiphobia = (treachery "60504" "Atychiphobia" 0)
  { cdCardTraits = setFromList [Madness]
  , cdRevelation = True
  }

ladyEsprit :: CardDef
ladyEsprit = (asset "81019" "Lady Espirt" 4 Neutral)
  { cdSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , cdCardTraits = setFromList [Ally, Sorcerer]
  }

bearTrap :: CardDef
bearTrap =
  (asset "81020" "Bear Trap" 0 Neutral) { cdCardTraits = setFromList [Trap] }

fishingNet :: CardDef
fishingNet = (asset "81021" "Fishing Net" 0 Neutral)
  { cdCardTraits = setFromList [Trap]
  }

curseOfTheRougarou :: CardDef
curseOfTheRougarou = (treachery "81029" "Curse of the Rougarou" 0
                            )
  { cdCardTraits = setFromList [Curse]
  , cdRevelation = True
  }

monstrousTransformation :: CardDef
monstrousTransformation =
  (asset "81030" "Monstrous Transformation" 0 Neutral)
    { cdCardTraits = setFromList [Talent]
    , cdFast = True
    , cdWindows = setFromList [DuringTurn You]
    }

daisysToteBagAdvanced :: CardDef
daisysToteBagAdvanced =
  (asset "90002" "Daisy's Tote Bag" 2 Neutral)
    { cdSkills = [SkillWillpower, SkillIntellect, SkillWild, SkillWild]
    , cdCardTraits = setFromList [Item]
    }

theNecronomiconAdvanced :: CardDef
theNecronomiconAdvanced =
  (asset "90003" "The Necronomicon" 0 Neutral)
    { cdCardTraits = setFromList [Item, Tome]
    , cdWeakness = True
    , cdRevelation = True
    }
