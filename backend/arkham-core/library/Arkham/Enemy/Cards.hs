module Arkham.Enemy.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding ( Byakhee )
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Keyword qualified as Keyword
import Arkham.Name
import Arkham.Trait

baseEnemy
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef k
baseEnemy cardCode name mEncounterSet isWeakness = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdAdditionalCost = Nothing
  , cdLevel = 0
  , cdCardSubType = isWeakness
  , cdClassSymbols = if isJust isWeakness then singleton Neutral else mempty
  , cdSkills = mempty
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
  , cdEncounterSet = fst <$> mEncounterSet
  , cdEncounterSetQuantity = snd <$> mEncounterSet
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

weakness :: CardCode -> Name -> CardDef 'PlayerEnemyType
weakness cardCode name = baseEnemy cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef 'PlayerEnemyType
basicWeakness cardCode name =
  baseEnemy cardCode name Nothing (Just BasicWeakness)

enemy :: CardCode -> Name -> EncounterSet -> Int -> (CardDef 'EnemyType)
enemy cardCode name encounterSet encounterSetQuantity =
  baseEnemy cardCode name (Just (encounterSet, encounterSetQuantity)) Nothing

allPlayerEnemyCards :: HashMap CardCode (CardDef 'PlayerEnemyType)
allPlayerEnemyCards = mapFromList $ concatMap
  toCardCodePairs
  [ mobEnforcer
  , silverTwilightAcolyte
  , stubbornDetective
  , graveyardGhouls
  , theThingThatFollows
  , theManInThePallidMask
  , serpentsOfYig
  , hoods
  , tommyMalloy
  ]

allEncounterEnemyCards :: HashMap CardCode (CardDef 'EnemyType)
allEncounterEnemyCards = mapFromList $ concatMap
  toCardCodePairs
  [ acolyte
  , acolyteOfUmordhoth
  , agentOfTheKing
  , alejandroVela
  , almaHill
  , apexStrangleweed
  , ashleighClarke
  , asylumGorger
  , avianThrall
  , balefulReveler
  , basilisk
  , beastOfAldebaran
  , billyCooper
  , boaConstrictor
  , bogGator
  , broodOfYig
  , broodOfYogSothoth
  , brotherhoodCultist
  , carnevaleSentinel
  , catacombsDocent
  , cloverClubPitBoss
  , cnidathqua
  , conglomerationOfSpheres
  , constanceDumaine
  , corpseDweller
  , corpseHungryGhoul
  , corpseTaker
  , crazedShoggoth
  , creatureOutOfDemhe
  , danielChesterfield
  , darkYoungHost
  , devoteeOfTheKey
  , dianneDevine
  , discipleOfTheDevourer
  , donLagorio
  , eaterOfTheDepths
  , elisabettaMagro
  , emergentMonstrosity
  , eztliGuardian
  , fanatic
  , fangOfYig
  , fleshEater
  , formlessSpawn
  , ghoulFromTheDepths
  , ghoulMinion
  , ghoulPriest
  , goatSpawn
  , grapplingHorror
  , graveEater
  , handOfTheBrotherhood
  , harbingerOfValusia
  , harlanEarnstoneCrazedByTheCurse
  , hasturLordOfCarcosa
  , hasturTheKingInYellow
  , hasturTheTatteredKing
  , henryDeveauAlejandrosKidnapper
  , hermanCollins
  , huntingHorror
  , huntingNightgaunt
  , ichtaca
  , ichtacaScionOfYig
  , icyGhoul
  , interstellarTraveler
  , ishimaruHaruko
  , jeremiahPierce
  , jordanPerry
  , keeperOfTheGreatLibrary
  , lupineThrall
  , madPatient
  , maniac
  , mariaDeSilvaKnowsMoreThanSheLetsOn
  , marshGug
  , mobster
  , narogath
  , oBannionsThug
  , padmaAmrita
  , peterWarren
  , pitViper
  , pitWarden
  , poleman
  , poltergeist
  , possessedOathspeaker
  , ravenousGhoul
  , relentlessDarkYoung
  , riftSeeker
  , roachSwarm
  , royalEmissary
  , ruthTurner
  , salvatoreNeri
  , savioCorvi
  , scholarFromYith
  , scientistOfYith
  , screechingByakhee
  , sebastienMoreau
  , seekerOfCarcosa
  , serpentFromYoth
  , serpentOfTenochtitlan
  , servantOfManyMouths
  , servantOfTheLurker
  , sethBishop
  , silasBishop
  , slimeCoveredDhole
  , spawnOfHali
  , specterOfDeath
  , stealthyByakhee
  , swampLeech
  , swarmOfRats
  , swiftByakhee
  , temporalDevourer
  , theExperiment
  , theMaskedHunter
  , theOrganistDrapedInMystery
  , theOrganistHopelessIDefiedHim
  , theRougarou
  , theWingedSerpent
  , thrall
  , tidalTerror
  , umordhoth
  , victoriaDevereux
  , whippoorwill
  , wingedOne
  , wizardOfTheOrder
  , wizardOfYogSothoth
  , wolfManDrew
  , writhingAppendage
  , yig
  , yithianObserver
  , yithianStarseeker
  , yogSothoth
  , youngDeepOne
  , youngPsychopath
  ]

allSpecialEnemyCards :: HashMap CardCode (CardDef 'EnemyType)
allSpecialEnemyCards = mapFrom toCardCode [flyingPolyp]

mobEnforcer :: CardDef 'PlayerEnemyType
mobEnforcer = (basicWeakness "01101" "Mob Enforcer")
  { cdCardTraits = setFromList [Humanoid, Criminal]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdAlternateCardCodes = ["01601"]
  }

silverTwilightAcolyte :: CardDef 'PlayerEnemyType
silverTwilightAcolyte = (basicWeakness "01102" "Silver Twilight Acolyte")
  { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdAlternateCardCodes = ["01602"]
  }

stubbornDetective :: CardDef 'PlayerEnemyType
stubbornDetective = (basicWeakness "01103" "Stubborn Detective")
  { cdCardTraits = setFromList [Humanoid, Detective]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdAlternateCardCodes = ["01603"]
  }

ghoulPriest :: CardDef 'EnemyType
ghoulPriest = (enemy "01116" "Ghoul Priest" TheGathering 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  , cdVictoryPoints = Just 2
  }

fleshEater :: CardDef 'EnemyType
fleshEater = (enemy "01118" "Flesh-Eater" TheGathering 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdVictoryPoints = Just 1
  }

icyGhoul :: CardDef 'EnemyType
icyGhoul = (enemy "01119" "Icy Ghoul" TheGathering 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdVictoryPoints = Just 1
  }

theMaskedHunter :: CardDef 'EnemyType
theMaskedHunter = (enemy
                    "01121b"
                    ("The Masked Hunter" <:> "Silently Stalking")
                    TheMidnightMasks
                    1
                  )
  { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 2
  , cdUnique = True
  }

wolfManDrew :: CardDef 'EnemyType
wolfManDrew =
  (enemy "01137" ("\"Wolf-Man\" Drew" <:> "The Cannibal") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

hermanCollins :: CardDef 'EnemyType
hermanCollins =
  (enemy "01138" ("Herman Collins" <:> "The Undertaker") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

peterWarren :: CardDef 'EnemyType
peterWarren =
  (enemy "01139" ("Peter Warren" <:> "The Occult Professor") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

victoriaDevereux :: CardDef 'EnemyType
victoriaDevereux =
  (enemy "01140" ("Victoria Devereux" <:> "The Collector") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

ruthTurner :: CardDef 'EnemyType
ruthTurner =
  (enemy "01141" ("Ruth Turner" <:> "The Mortician") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

umordhoth :: CardDef 'EnemyType
umordhoth =
  (enemy "01157" ("Umôrdhoth" <:> "The Devourer Below") TheDevourerBelow 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdUnique = True
    }

swarmOfRats :: CardDef 'EnemyType
swarmOfRats = (enemy "01159" "Swarm of Rats" Rats 3)
  { cdCardTraits = setFromList [Creature]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

ghoulMinion :: CardDef 'EnemyType
ghoulMinion = (enemy "01160" "Ghoul Minion" Ghouls 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

ravenousGhoul :: CardDef 'EnemyType
ravenousGhoul = (enemy "01161" "Ravenous Ghoul" Ghouls 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyte :: CardDef 'EnemyType
acolyte = (enemy "01169" "Acolyte" DarkCult 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  }

wizardOfTheOrder :: CardDef 'EnemyType
wizardOfTheOrder = (enemy "01170" "Wizard of the Order" DarkCult 1)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

huntingNightgaunt :: CardDef 'EnemyType
huntingNightgaunt = (enemy "01172" "Hunting Nightgaunt" Nightgaunts 2)
  { cdCardTraits = setFromList [Monster, Nightgaunt]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

screechingByakhee :: CardDef 'EnemyType
screechingByakhee = (enemy "01175" "Screeching Byakhee" AgentsOfHastur 2)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

yithianObserver :: CardDef 'EnemyType
yithianObserver = (enemy "01177" "Yithian Observer" AgentsOfYogSothoth 2)
  { cdCardTraits = setFromList [Monster, Yithian]
  , cdVictoryPoints = Just 1
  }

relentlessDarkYoung :: CardDef 'EnemyType
relentlessDarkYoung =
  (enemy "01179" "Relentless Dark Young" AgentsOfShubNiggurath 1)
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

goatSpawn :: CardDef 'EnemyType
goatSpawn = (enemy "01180" "Goat Spawn" AgentsOfShubNiggurath 3)
  { cdCardTraits = setFromList [Humanoid, Monster]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

youngDeepOne :: CardDef 'EnemyType
youngDeepOne = (enemy "01181" "Young Deep One" AgentsOfCthulhu 2)
  { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

theExperiment :: CardDef 'EnemyType
theExperiment = (enemy
                  "02058"
                  ("The Experiment" <:> "Something Went Terribly Wrong")
                  ExtracurricularActivity
                  1
                )
  { cdCardTraits = setFromList [Monster, Abomination, Elite]
  , cdKeywords = setFromList [Keyword.Massive]
  , cdVictoryPoints = Just 2
  , cdUnique = True
  }

cloverClubPitBoss :: CardDef 'EnemyType
cloverClubPitBoss = (enemy "02078" "Clover Club Pit Boss" TheHouseAlwaysWins 1)
  { cdCardTraits = setFromList [Humanoid, Criminal, Elite]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

thrall :: CardDef 'EnemyType
thrall = (enemy "02086" "Thrall" BishopsThralls 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

wizardOfYogSothoth :: CardDef 'EnemyType
wizardOfYogSothoth = (enemy "02087" "Wizard of Yog-Sothoth" BishopsThralls 1)
  { cdCardTraits = setFromList [Humanoid, Sorcerer]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

whippoorwill :: CardDef 'EnemyType
whippoorwill = (enemy "02090" "Whippoorwill" Whippoorwills 3)
  { cdCardTraits = setFromList [Creature]
  , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
  }

avianThrall :: CardDef 'EnemyType
avianThrall = (enemy "02094" "Avian Thrall" BeastThralls 2)
  { cdCardTraits = setFromList [Creature, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

lupineThrall :: CardDef 'EnemyType
lupineThrall = (enemy "02095" "Lupine Thrall" BeastThralls 2)
  { cdCardTraits = setFromList [Creature, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

oBannionsThug :: CardDef 'EnemyType
oBannionsThug = (enemy "02097" "O'Bannion's Thug" NaomisCrew 2)
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  }

mobster :: CardDef 'EnemyType
mobster = (enemy "02098" "Mobster" NaomisCrew 2)
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

conglomerationOfSpheres :: CardDef 'EnemyType
conglomerationOfSpheres =
  (enemy "02103" "Conglomeration of Spheres" HideousAbominations 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardDef 'EnemyType
servantOfTheLurker =
  (enemy "02104" "Servant of the Lurker" HideousAbominations 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingHorror :: CardDef 'EnemyType
huntingHorror = (enemy
                  "02141"
                  ("Hunting Horror" <:> "Spawned from the Void")
                  TheMiskatonicMuseum
                  1
                )
  { cdCardTraits = setFromList [Monster, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

grapplingHorror :: CardDef 'EnemyType
grapplingHorror = (enemy "02182" "Grappling Horror" TheEssexCountyExpress 2)
  { cdCardTraits = setFromList [Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

emergentMonstrosity :: CardDef 'EnemyType
emergentMonstrosity =
  (enemy "02183" "Emergent Monstrosity" TheEssexCountyExpress 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdVictoryPoints = Just 1
    }

silasBishop :: CardDef 'EnemyType
silasBishop =
  (enemy "02216" ("Silas Bishop" <:> "Infused With Evil") BloodOnTheAltar 1)
    { cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

servantOfManyMouths :: CardDef 'EnemyType
servantOfManyMouths = (enemy "02224" "Servant of Many Mouths" BloodOnTheAltar 3
                      )
  { cdCardTraits = singleton Humanoid
  , cdKeywords = singleton Keyword.Retaliate
  }

broodOfYogSothoth :: CardDef 'EnemyType
broodOfYogSothoth =
  (enemy "02255" "Brood of Yog-Sothoth" UndimensionedAndUnseen 5)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

sethBishop :: CardDef 'EnemyType
sethBishop =
  (enemy "02293" ("Seth Bishop" <:> "Sorcerer of Dunwich") WhereDoomAwaits 1)
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

devoteeOfTheKey :: CardDef 'EnemyType
devoteeOfTheKey = (enemy "02294" "Devotee of the Key" WhereDoomAwaits 2)
  { cdCardTraits = setFromList [Humanoid, Sorcerer]
  }

crazedShoggoth :: CardDef 'EnemyType
crazedShoggoth = (enemy "02295" "Crazed Shoggoth" WhereDoomAwaits 1)
  { cdCardTraits = setFromList [Monster, Shoggoth]
  , cdVictoryPoints = Just 1
  }

yogSothoth :: CardDef 'EnemyType
yogSothoth = (enemy
               "02323"
               ("Yog-Sothoth" <:> "The Lurker Beyond the Threshold")
               LostInTimeAndSpace
               1
             )
  { cdCardTraits = setFromList [AncientOne, Elite]
  , cdKeywords = setFromList
    [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
  , cdUnique = True
  }

interstellarTraveler :: CardDef 'EnemyType
interstellarTraveler =
  (enemy "02329" "Interstellar Traveler" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardDef 'EnemyType
yithianStarseeker = (enemy "02330" "Yithian Starseeker" LostInTimeAndSpace 2)
  { cdCardTraits = setFromList [Monster, Yithian]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

graveyardGhouls :: CardDef 'PlayerEnemyType
graveyardGhouls = (weakness "03017" "Graveyard Ghouls")
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

theThingThatFollows :: CardDef 'PlayerEnemyType
theThingThatFollows = (basicWeakness "03042" "The Thing That Follows")
  { cdCardTraits = setFromList [Monster, Curse]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdUnique = True
  }

theManInThePallidMask :: CardDef 'PlayerEnemyType
theManInThePallidMask = (weakness "03059" "The Man in the Pallid Mask")
  { cdCardTraits = setFromList [Humanoid, Elite]
  , cdKeywords = setFromList [Keyword.Aloof]
  , cdUnique = True
  , cdEncounterSet = Just CurtainCall
  , cdEncounterSetQuantity = Just 1
  }

royalEmissary :: CardDef 'EnemyType
royalEmissary =
  (enemy "03060" ("Royal Emissary" <:> "Messenger from Aldebaran") CurtainCall 1
    )
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList
      [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

constanceDumaine :: CardDef 'EnemyType
constanceDumaine =
  (enemy
      "03065b"
      ("Constance Dumaine" <:> "A Little Too Sociable")
      TheLastKing
      1
    )
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

jordanPerry :: CardDef 'EnemyType
jordanPerry =
  (enemy "03066b" ("Jordan Perry" <:> "An Imposing Presence") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

ishimaruHaruko :: CardDef 'EnemyType
ishimaruHaruko =
  (enemy "03067b" ("Ishimaru Haruku" <:> "Just Skin and Bones") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

sebastienMoreau :: CardDef 'EnemyType
sebastienMoreau =
  (enemy "03068b" ("Sebastien Moreau" <:> "Savage Hysteria") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

ashleighClarke :: CardDef 'EnemyType
ashleighClarke =
  (enemy "03069b" ("Ashleigh Clarke" <:> "Songs Die Unheard") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

dianneDevine :: CardDef 'EnemyType
dianneDevine =
  (enemy "03081" ("Dianne Devine" <:> "Mercurial and Mischevious") TheLastKing 1
    )
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = singleton Keyword.Aloof
    , cdUnique = True
    }

swiftByakhee :: CardDef 'EnemyType
swiftByakhee = (enemy "03086" "Swift Byakhee" EncounterSet.Byakhee 2)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

beastOfAldebaran :: CardDef 'EnemyType
beastOfAldebaran = (enemy "03088" "Beast of Aldebaran" InhabitantsOfCarcosa 1)
  { cdCardTraits = setFromList [Monster, Elite]
  , cdKeywords = singleton Keyword.Massive
  , cdVictoryPoints = Just 1
  }

spawnOfHali :: CardDef 'EnemyType
spawnOfHali = (enemy "03089" "Spawn of Hali" InhabitantsOfCarcosa 2)
  { cdCardTraits = singleton Monster
  , cdKeywords = singleton Keyword.Retaliate
  }

poltergeist :: CardDef 'EnemyType
poltergeist = (enemy "03093" "Poltergeist" Hauntings 2)
  { cdCardTraits = setFromList [Monster, Geist]
  }

maniac :: CardDef 'EnemyType
maniac = (enemy "03095" "Maniac" HastursGift 2)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

youngPsychopath :: CardDef 'EnemyType
youngPsychopath = (enemy "03096" "Young Psychopath" HastursGift 2)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

fanatic :: CardDef 'EnemyType
fanatic = (enemy "03098" "Fanatic" CultOfTheYellowSign 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  }

agentOfTheKing :: CardDef 'EnemyType
agentOfTheKing = (enemy "03099" "Agent of the King" CultOfTheYellowSign 1)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = singleton Keyword.Hunter
  , cdVictoryPoints = Just 1
  }

roachSwarm :: CardDef 'EnemyType
roachSwarm = (enemy "03103" "Roach Swarm" DecayAndFilth 2)
  { cdCardTraits = singleton Creature
  }

possessedOathspeaker :: CardDef 'EnemyType
possessedOathspeaker = (enemy
                         "03140"
                         ("Possessed Oathspeaker" <:> "A Damnable Fate")
                         EchoesOfThePast
                         1
                       )
  { cdCardTraits = setFromList [Monster, Servitor, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  , cdVictoryPoints = Just 2
  }

seekerOfCarcosa :: CardDef 'EnemyType
seekerOfCarcosa = (enemy "03144" "Seeker of Carcosa" EchoesOfThePast 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = singleton Keyword.Aloof
  }

danielChesterfield :: CardDef 'EnemyType
danielChesterfield = (enemy
                       "03182b"
                       ("Daniel Chesterfield"
                       <:> "...Or At Least, What's Left of Him"
                       )
                       TheUnspeakableOath
                       1
                     )
  { cdCardTraits = setFromList [Humanoid, Lunatic, Elite]
  , cdUnique = True
  , cdVictoryPoints = Just 1
  , cdDoubleSided = True
  }

asylumGorger :: CardDef 'EnemyType
asylumGorger = (enemy "03183" "Asylum Gorger" TheUnspeakableOath 2)
  { cdCardTraits = setFromList [Monster, Abomination]
  , cdKeywords = singleton Keyword.Hunter
  }

madPatient :: CardDef 'EnemyType
madPatient = (enemy "03184" "Mad Patient" TheUnspeakableOath 3)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

theOrganistHopelessIDefiedHim :: CardDef 'EnemyType
theOrganistHopelessIDefiedHim =
  (enemy
      "03221a"
      ("The Organist" <:> "Hopeless, I Defied Him")
      APhantomOfTruth
      1
    )
    { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    }

theOrganistDrapedInMystery :: CardDef 'EnemyType
theOrganistDrapedInMystery =
  (enemy "03221b" ("The Organist" <:> "Draped in Mystery") APhantomOfTruth 1)
    { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
    , cdKeywords = singleton Keyword.Aloof
    , cdUnique = True
    , cdDoubleSided = True
    }

stealthyByakhee :: CardDef 'EnemyType
stealthyByakhee = (enemy "03222" "Stealthy Byakhee" APhantomOfTruth 2)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = singleton Keyword.Hunter
  }

specterOfDeath :: CardDef 'EnemyType
specterOfDeath =
  (enemy "03241b" ("Specter of Death" <:> "A Force From Beyond") ThePallidMask 1
    )
    { cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    }

catacombsDocent :: CardDef 'EnemyType
catacombsDocent = (enemy "03258" "Catacombs Docent" ThePallidMask 3)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

corpseDweller :: CardDef 'EnemyType
corpseDweller = (enemy "03259" "Corpse Dweller" ThePallidMask 3)
  { cdCardTraits = singleton Monster
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

tidalTerror :: CardDef 'EnemyType
tidalTerror = (enemy "03300" "Tidal Terror" BlackStarsRise 2)
  { cdCardTraits = singleton Monster
  , cdKeywords = singleton Keyword.Hunter
  }

riftSeeker :: CardDef 'EnemyType
riftSeeker = (enemy "03301" "Rift Seeker" BlackStarsRise 2)
  { cdCardTraits = setFromList [Monster, Byakhee, Cultist]
  }

hasturTheKingInYellow :: CardDef 'EnemyType
hasturTheKingInYellow =
  (enemy "03332" ("Hastur" <:> "The King in Yellow") DimCarcosa 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdUnique = True
    }

hasturLordOfCarcosa :: CardDef 'EnemyType
hasturLordOfCarcosa =
  (enemy "03333" ("Hastur" <:> "Lord of Carcosa") DimCarcosa 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdUnique = True
    }

hasturTheTatteredKing :: CardDef 'EnemyType
hasturTheTatteredKing =
  (enemy "03334" ("Hastur" <:> "The Tattered King") DimCarcosa 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    }

creatureOutOfDemhe :: CardDef 'EnemyType
creatureOutOfDemhe = (enemy "03335" "Creature Out of Demhe" DimCarcosa 1)
  { cdCardTraits = singleton Monster
  , cdKeywords = singleton Keyword.Massive
  }

wingedOne :: CardDef 'EnemyType
wingedOne = (enemy "03336" "Winged One" DimCarcosa 1)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = singleton Keyword.Retaliate
  }

serpentsOfYig :: CardDef 'PlayerEnemyType
serpentsOfYig = (weakness "04014" "Serpents of Yig")
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdKeywords = singleton Keyword.Hunter
  , cdRevelation = True
  }

ichtaca :: CardDef 'EnemyType
ichtaca =
  (enemy "04052" ("Ichtaca" <:> "Keeper of the Eztli") TheUntamedWilds 1)
    { cdCardTraits = setFromList [Humanoid, Eztli, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

harbingerOfValusia :: CardDef 'EnemyType
harbingerOfValusia = (enemy
                       "04062"
                       ("Harbinger of Valusia" <:> "The Sleeper Awakens")
                       TheDoomOfEztli
                       1
                     )
  { cdCardTraits = setFromList [Humanoid, Serpent, Monster, Elite]
  , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
  , cdVengeancePoints = Just 5
  , cdUnique = True
  }

pitViper :: CardDef 'EnemyType
pitViper = (enemy "04078" "Pit Viper" Serpents 3)
  { cdCardTraits = setFromList [Creature, Serpent]
  , cdVengeancePoints = Just 1
  }

boaConstrictor :: CardDef 'EnemyType
boaConstrictor = (enemy "04079" "Boa Constrictor" Serpents 1)
  { cdCardTraits = setFromList [Creature, Serpent]
  , cdKeywords = singleton Keyword.Hunter
  , cdVengeancePoints = Just 2
  }

broodOfYig :: CardDef 'EnemyType
broodOfYig = (enemy "04083" "Brood of Yig" AgentsOfYig 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdKeywords = singleton Keyword.Hunter
  }

serpentFromYoth :: CardDef 'EnemyType
serpentFromYoth = (enemy "04084" "Serpent from Yoth" AgentsOfYig 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdVictoryPoints = Just 1
  }

eztliGuardian :: CardDef 'EnemyType
eztliGuardian = (enemy "04086" "Eztli Guardian" GuardiansOfTime 2)
  { cdCardTraits = setFromList [Humanoid, Eztli]
  , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
  }

brotherhoodCultist :: CardDef 'EnemyType
brotherhoodCultist = (enemy "04095" "Brotherhood Cultist" PnakoticBrotherhood 2
                     )
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = singleton Keyword.Hunter
  }

fangOfYig :: CardDef 'EnemyType
fangOfYig = (enemy "04098" "Fang of Yig" YigsVenom 2)
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdKeywords = singleton Keyword.Retaliate
  }

harlanEarnstoneCrazedByTheCurse :: CardDef 'EnemyType
harlanEarnstoneCrazedByTheCurse =
  (enemy "04122b" ("Harlan Earnstone" <:> "Crazed by the Curse") ThreadsOfFate 1
    )
    { cdCardTraits = setFromList [Humanoid, Cursed, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

henryDeveauAlejandrosKidnapper :: CardDef 'EnemyType
henryDeveauAlejandrosKidnapper =
  (enemy "04130d" ("Henry Deveau" <:> "Alejandro's Kidnapper") ThreadsOfFate 1
    )
    { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

mariaDeSilvaKnowsMoreThanSheLetsOn :: CardDef 'EnemyType
mariaDeSilvaKnowsMoreThanSheLetsOn =
  (enemy "04137b" ("Maria DeSilva" <:> "Knows More Than She Lets On") ThreadsOfFate 1
    )
    { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    , cdKeywords = singleton Keyword.Retaliate
    }

padmaAmrita :: CardDef 'EnemyType
padmaAmrita =
  (enemy "04186" ("Padma Amrita" <:> "Cold-Blooded Charmer") TheBoundaryBeyond 1
    )
    { cdCardTraits = setFromList [Humanoid, Serpent, Servitor, Elite]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    , cdVengeancePoints = Just 2
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate, Keyword.Hunter]
    }

serpentOfTenochtitlan :: CardDef 'EnemyType
serpentOfTenochtitlan =
  (enemy "04187" "Serpent of Tenochtitlán" TheBoundaryBeyond 1
    )
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVictoryPoints = Just 1
    , cdVengeancePoints = Just 1
    }

handOfTheBrotherhood :: CardDef 'EnemyType
handOfTheBrotherhood =
  (enemy "04188" "Hand of the Brotherhood" TheBoundaryBeyond 2
    )
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

theWingedSerpent :: CardDef 'EnemyType
theWingedSerpent =
  (enemy "04209b" ("The Winged Serpent" <:> "The Wrath of Yig") PillarsOfJudgement 1
    )
    { cdCardTraits = setFromList [Monster, Serpent, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive]
    , cdUnique = True
    }

apexStrangleweed :: CardDef 'EnemyType
apexStrangleweed =
  (enemy "04219" "Apex Strangleweed" PillarsOfJudgement 2
    )
    { cdCardTraits = setFromList [Creature, Flora]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    }

basilisk :: CardDef 'EnemyType
basilisk =
  (enemy "04220" "Basilisk" PillarsOfJudgement 2
    )
    { cdCardTraits = setFromList [Monster, Serpent]
    , cdKeywords = singleton Keyword.Hunter
    , cdVengeancePoints = Just 2
    }

keeperOfTheGreatLibrary :: CardDef 'EnemyType
keeperOfTheGreatLibrary =
  (enemy "04257" "Keeper of the Great Library" TheCityOfArchives 2
    )
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }

scientistOfYith :: CardDef 'EnemyType
scientistOfYith =
  (enemy "04258" "Scientist of Yith" TheCityOfArchives 2
    )
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = singleton Keyword.Aloof
    }

scholarFromYith :: CardDef 'EnemyType
scholarFromYith =
  (enemy "04259" "Scholar from Yith" TheCityOfArchives 3
    )
    { cdCardTraits = setFromList [Monster, Yithian]
    }

yig :: CardDef 'EnemyType
yig =
  (enemy "04296" ("Yig" <:> "The Father of Serpents") TheDepthsOfYoth 1
    )
    { cdCardTraits = setFromList [AncientOne, Serpent, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 5
    , cdUnique = True
    }

pitWarden :: CardDef 'EnemyType
pitWarden =
  (enemy "04297" "Pit Warden" TheDepthsOfYoth 3
    )
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdVengeancePoints = Just 1
    }

eaterOfTheDepths :: CardDef 'EnemyType
eaterOfTheDepths =
  (enemy "04298" "Eater of the Depths" TheDepthsOfYoth 1
    )
    { cdCardTraits = singleton Monster
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }

ichtacaScionOfYig :: CardDef 'EnemyType
ichtacaScionOfYig =
  (enemy "04325" ("Ichtaca" <:> "Scion of Yig") ShatteredAeons 1
    )
    { cdCardTraits = setFromList [Humanoid, Monster, Serpent, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

alejandroVela :: CardDef 'EnemyType
alejandroVela =
  (enemy "04326" ("Alejandro Vela" <:> "Or, Is He?") ShatteredAeons 1
    )
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

formlessSpawn :: CardDef 'EnemyType
formlessSpawn =
  (enemy "04337" "Formless Spawn" ShatteredAeons 1
    )
    { cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 2
    }

temporalDevourer :: CardDef 'EnemyType
temporalDevourer =
  (enemy "04338" "Temporal Devourer" ShatteredAeons 2
    )
    { cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = singleton Keyword.Hunter
    }

hoods :: CardDef 'PlayerEnemyType
hoods = (weakness "05017" "Hoods")
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
  }

corpseHungryGhoul :: CardDef 'EnemyType
corpseHungryGhoul = (enemy "50022" "Corpse-Hungry Ghoul" ReturnToTheGathering 1
                    )
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

ghoulFromTheDepths :: CardDef 'EnemyType
ghoulFromTheDepths =
  (enemy "50023" "Ghoul from the Depths" ReturnToTheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

narogath :: CardDef 'EnemyType
narogath =
  (enemy "50026b" ("Narôgath" <:> "The Charnel Lord") ReturnToTheMidnightMasks 1
    )
    { cdCardTraits = setFromList [Humanoid, Monster, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

graveEater :: CardDef 'EnemyType
graveEater = (enemy "50038" "Grave-Eater" GhoulsOfUmordhoth 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyteOfUmordhoth :: CardDef 'EnemyType
acolyteOfUmordhoth = (enemy "50039" "Acolyte of Umôrdhoth" GhoulsOfUmordhoth 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

discipleOfTheDevourer :: CardDef 'EnemyType
discipleOfTheDevourer =
  (enemy "50041" "Disciple of the Devourer" TheDevourersCult 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

corpseTaker :: CardDef 'EnemyType
corpseTaker = (enemy "50042" "Corpse-Taker" TheDevourersCult 1)
  { cdCardTraits = setFromList [Monster, Servitor, Cultist]
  }

jeremiahPierce :: CardDef 'EnemyType
jeremiahPierce = (enemy
                   "50044"
                   ("Jeremiah Pierce" <:> "Your Next-Door Neighbor")
                   ReturnCultOfUmordhoth
                   1
                 )
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

billyCooper :: CardDef 'EnemyType
billyCooper =
  (enemy "50045" ("Billy Cooper" <:> "The Crooked Cop") ReturnCultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

almaHill :: CardDef 'EnemyType
almaHill = (enemy
             "50046"
             ("Alma Hill" <:> "The Inquisitive Historian")
             ReturnCultOfUmordhoth
             1
           )
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

tommyMalloy :: CardDef 'PlayerEnemyType
tommyMalloy = (weakness "60103" "Tommy Malloy")
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdUnique = True
  }

bogGator :: CardDef 'EnemyType
bogGator = (enemy "81022" "Bog Gator" TheBayou 2)
  { cdCardTraits = setFromList [Creature]
  }

swampLeech :: CardDef 'EnemyType
swampLeech = (enemy "81023" "Swamp Leech" TheBayou 3)
  { cdCardTraits = setFromList [Creature]
  }

theRougarou :: CardDef 'EnemyType
theRougarou =
  (enemy "81028" ("The Rougarou" <:> "Cursed Soul") CurseOfTheRougarou 1)
    { cdCardTraits = setFromList [Monster, Creature, Elite]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    , cdUnique = True
    }

slimeCoveredDhole :: CardDef 'EnemyType
slimeCoveredDhole = (enemy "81031" "Slime-Covered Dhole" CurseOfTheRougarou 2)
  { cdCardTraits = setFromList [Monster, Dhole]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

marshGug :: CardDef 'EnemyType
marshGug = (enemy "81032" "Marsh Gug" CurseOfTheRougarou 2)
  { cdCardTraits = setFromList [Monster, Gug]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

darkYoungHost :: CardDef 'EnemyType
darkYoungHost = (enemy "81033" "Dark Young Host" CurseOfTheRougarou 1)
  { cdCardTraits = setFromList [Monster, DarkYoung]
  , cdVictoryPoints = Just 1
  }

balefulReveler :: CardDef 'EnemyType
balefulReveler =
  (enemy "82002b" ("Baleful Reveler" <:> "Spreading Chaos") CarnevaleOfHorrors 1
    )
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

donLagorio :: CardDef 'EnemyType
donLagorio =
  (enemy "82017" ("Don Lagorio" <:> "Secret Servant") CarnevaleOfHorrors 1)
    { cdCardTraits = setFromList [Humanoid, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

elisabettaMagro :: CardDef 'EnemyType
elisabettaMagro = (enemy
                    "82018"
                    ("Elisabetta Magro" <:> "High Servant of the Order")
                    CarnevaleOfHorrors
                    1
                  )
  { cdCardTraits = setFromList [Humanoid, Lodge, Elite]
  , cdKeywords = setFromList [Keyword.Aloof]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

salvatoreNeri :: CardDef 'EnemyType
salvatoreNeri = (enemy
                  "82019"
                  ("Salvatore Neri" <:> "Master of Illusions")
                  CarnevaleOfHorrors
                  1
                )
  { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
  , cdKeywords = setFromList [Keyword.Retaliate]
  , cdVictoryPoints = Just 1
  , cdUnique = True
  }

savioCorvi :: CardDef 'EnemyType
savioCorvi =
  (enemy "82020" ("Savio Corvi" <:> "Dark Lurker") CarnevaleOfHorrors 1)
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

cnidathqua :: CardDef 'EnemyType
cnidathqua =
  (enemy "82027" ("Cnidathqua" <:> "The Many-armed Beast") CarnevaleOfHorrors 1)
    { cdCardTraits = setFromList [Monster, AncientOne, Elite]
    , cdUnique = True
    }

poleman :: CardDef 'EnemyType
poleman = (enemy "82028" "Poleman" CarnevaleOfHorrors 2)
  { cdCardTraits = setFromList [Monster, DeepOne]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

carnevaleSentinel :: CardDef 'EnemyType
carnevaleSentinel = (enemy "82029" "Carnevale Sentinel" CarnevaleOfHorrors 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

writhingAppendage :: CardDef 'EnemyType
writhingAppendage = (enemy "82030" "Writhing Appendage" CarnevaleOfHorrors 3)
  { cdCardTraits = setFromList [Monster, Tentacle]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

flyingPolyp :: CardDef 'EnemyType
flyingPolyp = (enemy "xpolyp" "Flying Polyp" ShatteredAeons 0)
  { cdCardTraits = singleton Monster
  }
