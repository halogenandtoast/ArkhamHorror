module Arkham.Enemy.Cards where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.EncounterSet
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.Trait

baseEnemy :: CardCode -> Name -> Maybe EncounterSet -> Bool -> CardDef
baseEnemy cardCode name mEncounterSet isWeakness = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = EnemyType
  , cdWeakness = isWeakness
  , cdClassSymbol = Nothing
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
  , cdEncounterSet = mEncounterSet
  , cdUnique = False
  }

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseEnemy cardCode name Nothing True

enemy :: CardCode -> Name -> EncounterSet -> CardDef
enemy cardCode name encounterSet = baseEnemy cardCode name (Just encounterSet) False

allPlayerEnemyCards :: HashMap CardCode CardDef
allPlayerEnemyCards = mapFromList
  [ ("01101", mobEnforcer)
  , ("01102", silverTwilightAcolyte)
  , ("01103", stubbornDetective)
  ]

allEncounterEnemyCards :: HashMap CardCode CardDef
allEncounterEnemyCards = mapFromList
  [ ("enemy", placeholderEnemy)
  , ("01116", ghoulPriest)
  , ("01118", fleshEater)
  , ("01119", icyGhoul)
  , ("01121b", theMaskedHunter)
  , ("01137", wolfManDrew)
  , ("01138", hermanCollins)
  , ("01139", peterWarren)
  , ("01140", victoriaDevereux)
  , ("01141", ruthTurner)
  , ("01157", umordhoth)
  , ("01159", swarmOfRats)
  , ("01160", ghoulMinion)
  , ("01161", ravenousGhoul)
  , ("01169", acolyte)
  , ("01170", wizardOfTheOrder)
  , ("01172", huntingNightgaunt)
  , ("01175", screechingByakhee)
  , ("01177", yithianObserver)
  , ("01179", relentlessDarkYoung)
  , ("01180", goatSpawn)
  , ("01181", youngDeepOne)
  , ("02058", theExperiment)
  , ("02078", cloverClubPitBoss)
  , ("02086", thrall)
  , ("02087", wizardOfYogSothoth)
  , ("02090", whippoorwill)
  , ("02094", avianThrall)
  , ("02095", lupineThrall)
  , ("02097", oBannionsThug)
  , ("02098", mobster)
  , ("02103", conglomerationOfSpheres)
  , ("02104", servantOfTheLurker)
  , ("02141", huntingHorror)
  , ("02182", grapplingHorror)
  , ("02183", emergentMonstrosity)
  , ("02216", silasBishop)
  , ("02224", servantOfManyMouths)
  , ("02255", broodOfYogSothoth)
  , ("02293", sethBishop)
  , ("02294", devoteeOfTheKey)
  , ("02295", crazedShoggoth)
  , ("02323", yogSothoth)
  , ("02329", interstellarTraveler)
  , ("02330", yithianStarseeker)
  , ("50022", corpseHungryGhoul)
  , ("50023", ghoulFromTheDepths)
  , ("50026b", narogath)
  , ("50038", graveEater)
  , ("50039", acolyteOfUmordhoth)
  , ("50041", discipleOfTheDevourer)
  , ("50042", corpseTaker)
  , ("50044", jeremiahPierce)
  , ("50045", billyCooper)
  , ("50046", almaHill)
  , ("81022", bogGator)
  , ("81023", swampLeech)
  , ("81028", theRougarou)
  , ("81031", slimeCoveredDhole)
  , ("81032", marshGug)
  , ("81033", darkYoungHost)
  ]

placeholderEnemy :: CardDef
placeholderEnemy = enemy "enemy" "Placeholder Enemy Card" Test

mobEnforcer :: CardDef
mobEnforcer = (weakness "01101" "Mob Enforcer")
  { cdCardTraits = setFromList [Humanoid, Criminal]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

silverTwilightAcolyte :: CardDef
silverTwilightAcolyte =
  (weakness "01102" "Silver Twilight Acolyte")
    { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

stubbornDetective :: CardDef
stubbornDetective = (weakness "01103" "Stubborn Detective")
  { cdCardTraits = setFromList [Humanoid, Detective]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

ghoulPriest :: CardDef
ghoulPriest = (enemy "01116" "Ghoul Priest" TheGathering)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  , cdVictoryPoints = Just 2
  }

fleshEater :: CardDef
fleshEater = (enemy "01118" "Flesh-Eater" TheGathering)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdVictoryPoints = Just 1
  }

icyGhoul :: CardDef
icyGhoul = (enemy "01119" "Icy Ghoul" TheGathering)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdVictoryPoints = Just 1
  }

theMaskedHunter :: CardDef
theMaskedHunter =
  (enemy "01121b" ("The Masked Hunter" <:> "Silently Stalking") TheMidnightMasks)
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

wolfManDrew :: CardDef
wolfManDrew = (enemy "01137" ("\"Wolf-Man\" Drew" <:> "The Cannibal") TheMidnightMasks)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

hermanCollins :: CardDef
hermanCollins = (enemy "01138" ("Herman Collins" <:> "The Undertaker") TheMidnightMasks)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

peterWarren :: CardDef
peterWarren = (enemy "01139" ("Peter Warren" <:> "The Occult Professor") TheMidnightMasks)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

victoriaDevereux :: CardDef
victoriaDevereux =
  (enemy "01140" ("Victoria Devereux" <:> "The Collector") TheMidnightMasks)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

ruthTurner :: CardDef
ruthTurner = (enemy "01141" ("Ruth Turner" <:> "The Mortician") TheMidnightMasks)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

umordhoth :: CardDef
umordhoth = (enemy "01157" ("Umôrdhoth" <:> "The Devourer Below") TheDevourerBelow)
  { cdCardTraits = setFromList [AncientOne, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
  , cdUnique = True
  }

swarmOfRats :: CardDef
swarmOfRats = (enemy "01159" "Swarm of Rats" Rats)
  { cdCardTraits = setFromList [Creature]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

ghoulMinion :: CardDef
ghoulMinion = (enemy "01160" "Ghoul Minion" Ghouls)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

ravenousGhoul :: CardDef
ravenousGhoul = (enemy "01161" "Ravenous Ghoul" Ghouls)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyte :: CardDef
acolyte = (enemy "01169" "Acolyte" DarkCult)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  }

wizardOfTheOrder :: CardDef
wizardOfTheOrder =
  (enemy "01170" "Wizard of the Order" DarkCult)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

huntingNightgaunt :: CardDef
huntingNightgaunt =
  (enemy "01172" "Hunting Nightgaunt" Nightgaunts)
    { cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

screechingByakhee :: CardDef
screechingByakhee =
  (enemy "01175" "Screeching Byakhee" AgentsOfHastur)
    { cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianObserver :: CardDef
yithianObserver =
  (enemy "01177" "Yithian Observer" AgentsOfYogSothoth)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdVictoryPoints = Just 1
    }

relentlessDarkYoung :: CardDef
relentlessDarkYoung =
  (enemy "01179" "Relentless Dark Young" AgentsOfShubNiggurath)
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

goatSpawn :: CardDef
goatSpawn = (enemy "01180" "Goat Spawn" AgentsOfShubNiggurath)
  { cdCardTraits = setFromList [Humanoid, Monster]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

youngDeepOne :: CardDef
youngDeepOne = (enemy "01181" "Young Deep One" AgentsOfCthulhu)
  { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

theExperiment :: CardDef
theExperiment = (enemy "02058" ("The Experiment" <:> "Something Went Terribly Wrong") ExtracurricularActivity)
  { cdCardTraits = setFromList [Monster, Abomination, Elite]
  , cdKeywords = setFromList [Keyword.Massive]
  , cdVictoryPoints = Just 2
  , cdUnique = True
  }

cloverClubPitBoss :: CardDef
cloverClubPitBoss =
  (enemy "02078" "Clover Club Pit Boss" TheHouseAlwaysWins)
    { cdCardTraits = setFromList [Criminal, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

thrall :: CardDef
thrall = (enemy "02086" "Thrall" BishopsThralls)
  { cdCardTraits = setFromList [Humanoid, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

wizardOfYogSothoth :: CardDef
wizardOfYogSothoth =
  (enemy "02087" "Wizard of Yog-Sothoth" BishopsThralls)
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

whippoorwill :: CardDef
whippoorwill = (enemy "02090" "Whippoorwill" Whippoorwills)
  { cdCardTraits = setFromList [Creature]
  , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
  }

avianThrall :: CardDef
avianThrall = (enemy "02094" "Avian Thrall" BeastThralls)
  { cdCardTraits = setFromList [Creature, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

lupineThrall :: CardDef
lupineThrall = (enemy "02095" "Lupine Thrall" BeastThralls)
  { cdCardTraits = setFromList [Creature, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

oBannionsThug :: CardDef
oBannionsThug = (enemy "02097" "O'Bannion's Thug" NaomisCrew)
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  }

mobster :: CardDef
mobster = (enemy "02098" "Mobster" NaomisCrew)
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

conglomerationOfSpheres :: CardDef
conglomerationOfSpheres =
  (enemy "02103" "Conglomeration of Spheres" HideousAbominations)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardDef
servantOfTheLurker =
  (enemy "02104" "Servant of the Lurker" HideousAbominations)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingHorror :: CardDef
huntingHorror = (enemy "02141" ("Hunting Horror" <:> "Spawned from the Void") TheMiskatonicMuseum)
  { cdCardTraits = setFromList [Monster, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

grapplingHorror :: CardDef
grapplingHorror =
  (enemy "02182" "Grappling Horror" TheEssexCountyExpress)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

emergentMonstrosity :: CardDef
emergentMonstrosity =
  (enemy "02183" "Emergent Monstrosity" TheEssexCountyExpress)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdVictoryPoints = Just 1
    }

silasBishop :: CardDef
silasBishop = (enemy "02216" ("Silas Bishop" <:> "Infused With Evil") BloodOnTheAltar)
  { cdCardTraits = setFromList [Monster, Abomination, Elite]
  , cdKeywords = singleton Keyword.Massive
  , cdVictoryPoints = Just 2
  , cdUnique = True
  }

servantOfManyMouths :: CardDef
servantOfManyMouths =
  (enemy "02224" "Servant of Many Mouths" BloodOnTheAltar)
    { cdCardTraits = singleton Humanoid
    , cdKeywords = singleton Keyword.Retaliate
    }

broodOfYogSothoth :: CardDef
broodOfYogSothoth =
  (enemy "02255" "Brood of Yog-Sothoth" UndimensionedAndUnseen)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

sethBishop :: CardDef
sethBishop =
  (enemy "02293" ("Seth Bishop" <:> "Sorcerer of Dunwich") WhereDoomAwaits)
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

devoteeOfTheKey :: CardDef
devoteeOfTheKey =
  (enemy "02294" "Devotee of the Key" WhereDoomAwaits)
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    }

crazedShoggoth :: CardDef
crazedShoggoth = (enemy "02295" "Crazed Shoggoth" WhereDoomAwaits)
  { cdCardTraits = setFromList [Monster, Shoggoth]
  , cdVictoryPoints = Just 1
  }

yogSothoth :: CardDef
yogSothoth = (enemy
                      "02323"
                      ("Yog-Sothoth"
                        <:> "The Lurker Beyond the Threshold")
                      LostInTimeAndSpace
                      )
  { cdCardTraits = setFromList [AncientOne, Elite]
  , cdKeywords = setFromList
    [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
  , cdUnique = True
  }

interstellarTraveler :: CardDef
interstellarTraveler =
  (enemy "02329" "Interstellar Traveler" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardDef
yithianStarseeker =
  (enemy "02330" "Yithian Starseeker" LostInTimeAndSpace)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

corpseHungryGhoul :: CardDef
corpseHungryGhoul =
  (enemy "50022" "Corpse-Hungry Ghoul" ReturnToTheGathering)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

ghoulFromTheDepths :: CardDef
ghoulFromTheDepths =
  (enemy "50023" "Ghoul from the Depths" ReturnToTheGathering)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

narogath :: CardDef
narogath = (enemy "50026b" ("Narôgath" <:> "The Charnel Lord") ReturnToTheMidnightMasks)
  { cdCardTraits = setFromList [Humanoid, Monster, Cultist, Elite]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 2
  , cdUnique = True
  }

graveEater :: CardDef
graveEater = (enemy "50038" "Grave-Eater" GhoulsOfUmordhoth)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyteOfUmordhoth :: CardDef
acolyteOfUmordhoth =
  (enemy "50039" "Acolyte of Umôrdhoth" GhoulsOfUmordhoth)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    }

discipleOfTheDevourer :: CardDef
discipleOfTheDevourer =
  (enemy "50041" "Disciple of the Devourer" TheDevourersCult)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

corpseTaker :: CardDef
corpseTaker = (enemy "50042" "Corpse-Taker" TheDevourersCult)
  { cdCardTraits = setFromList [Monster, Servitor, Cultist]
  }

jeremiahPierce :: CardDef
jeremiahPierce = (enemy "50044" ("Jeremiah Pierce" <:> "Your Next-Door Neighbor") ReturnCultOfUmordhoth)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

billyCooper :: CardDef
billyCooper = (enemy "50045" ("Billy Cooper" <:> "The Crooked Cop") ReturnCultOfUmordhoth)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

almaHill :: CardDef
almaHill = (enemy "50046" ("Alma Hill" <:> "The Inquisitive Historian") ReturnCultOfUmordhoth)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

bogGator :: CardDef
bogGator = (enemy "81022" "Bog Gator" TheBayou)
  { cdCardTraits = setFromList [Creature]
  }

swampLeech :: CardDef
swampLeech = (enemy "81023" "Swamp Leech" TheBayou)
  { cdCardTraits = setFromList [Creature]
  }

theRougarou :: CardDef
theRougarou = (enemy "81028" ("The Rougarou" <:> "Cursed Soul") CurseOfTheRougarou)
  { cdCardTraits = setFromList [Monster, Creature, Elite]
  , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
  , cdUnique = True
  }

slimeCoveredDhole :: CardDef
slimeCoveredDhole =
  (enemy "81031" "Slime-Covered Dhole" CurseOfTheRougarou)
    { cdCardTraits = setFromList [Monster, Dhole]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

marshGug :: CardDef
marshGug = (enemy "81032" "Marsh Gug" CurseOfTheRougarou)
  { cdCardTraits = setFromList [Monster, Gug]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

darkYoungHost :: CardDef
darkYoungHost = (enemy "81033" "Dark Young Host" CurseOfTheRougarou)
  { cdCardTraits = setFromList [Monster, DarkYoung]
  , cdVictoryPoints = Just 1
  }
