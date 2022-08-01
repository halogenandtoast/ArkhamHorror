module Arkham.Enemy.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (Byakhee)
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Keyword qualified as Keyword
import Arkham.Name
import Arkham.Trait

baseEnemy
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef
baseEnemy cardCode name mEncounterSet isWeakness = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdRevealedName = Nothing
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = if isJust isWeakness then PlayerEnemyType else EnemyType
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
  }

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseEnemy cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  baseEnemy cardCode name Nothing (Just BasicWeakness)

enemy :: CardCode -> Name -> EncounterSet -> Int -> CardDef
enemy cardCode name encounterSet encounterSetQuantity =
  baseEnemy cardCode name (Just (encounterSet, encounterSetQuantity)) Nothing

allPlayerEnemyCards :: HashMap CardCode CardDef
allPlayerEnemyCards = mapFromList $ concatMap
  toCardCodePairs
  [ mobEnforcer
  , silverTwilightAcolyte
  , stubbornDetective
  , graveyardGhouls
  , theThingThatFollows
  , theManInThePallidMask
  , serpentsOfYig
  , tommyMalloy
  ]

allEncounterEnemyCards :: HashMap CardCode CardDef
allEncounterEnemyCards = mapFromList $ concatMap
  toCardCodePairs
  [ acolyte
  , acolyteOfUmordhoth
  , agentOfTheKing
  , almaHill
  , ashleighClarke
  , asylumGorger
  , avianThrall
  , balefulReveler
  , beastOfAldebaran
  , billyCooper
  , boaConstrictor
  , bogGator
  , broodOfYig
  , broodOfYogSothoth
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
  , elisabettaMagro
  , emergentMonstrosity
  , eztliGuardian
  , fanatic
  , fleshEater
  , ghoulFromTheDepths
  , ghoulMinion
  , ghoulPriest
  , goatSpawn
  , grapplingHorror
  , graveEater
  , hasturLordOfCarcosa
  , hasturTheKingInYellow
  , hasturTheTatteredKing
  , hermanCollins
  , huntingHorror
  , huntingNightgaunt
  , ichtaca
  , icyGhoul
  , interstellarTraveler
  , ishimaruHaruko
  , jeremiahPierce
  , jordanPerry
  , lupineThrall
  , madPatient
  , maniac
  , marshGug
  , mobster
  , narogath
  , oBannionsThug
  , peterWarren
  , pitViper
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
  , screechingByakhee
  , sebastienMoreau
  , seekerOfCarcosa
  , serpentFromYoth
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
  , theExperiment
  , theMaskedHunter
  , theOrganistDrapedInMystery
  , theOrganistHopelessIDefiedHim
  , theRougarou
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
  , yithianObserver
  , yithianStarseeker
  , yogSothoth
  , youngDeepOne
  , youngPsychopath
  ]

mobEnforcer :: CardDef
mobEnforcer = (basicWeakness "01101" "Mob Enforcer")
  { cdCardTraits = setFromList [Humanoid, Criminal]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdAlternateCardCodes = ["01601"]
  }

silverTwilightAcolyte :: CardDef
silverTwilightAcolyte = (basicWeakness "01102" "Silver Twilight Acolyte")
  { cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdAlternateCardCodes = ["01602"]
  }

stubbornDetective :: CardDef
stubbornDetective = (basicWeakness "01103" "Stubborn Detective")
  { cdCardTraits = setFromList [Humanoid, Detective]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdAlternateCardCodes = ["01603"]
  }

ghoulPriest :: CardDef
ghoulPriest = (enemy "01116" "Ghoul Priest" TheGathering 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  , cdVictoryPoints = Just 2
  }

fleshEater :: CardDef
fleshEater = (enemy "01118" "Flesh-Eater" TheGathering 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdVictoryPoints = Just 1
  }

icyGhoul :: CardDef
icyGhoul = (enemy "01119" "Icy Ghoul" TheGathering 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdVictoryPoints = Just 1
  }

theMaskedHunter :: CardDef
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

wolfManDrew :: CardDef
wolfManDrew =
  (enemy "01137" ("\"Wolf-Man\" Drew" <:> "The Cannibal") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

hermanCollins :: CardDef
hermanCollins =
  (enemy "01138" ("Herman Collins" <:> "The Undertaker") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

peterWarren :: CardDef
peterWarren =
  (enemy "01139" ("Peter Warren" <:> "The Occult Professor") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

victoriaDevereux :: CardDef
victoriaDevereux =
  (enemy "01140" ("Victoria Devereux" <:> "The Collector") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

ruthTurner :: CardDef
ruthTurner =
  (enemy "01141" ("Ruth Turner" <:> "The Mortician") CultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

umordhoth :: CardDef
umordhoth =
  (enemy "01157" ("Umôrdhoth" <:> "The Devourer Below") TheDevourerBelow 1)
    { cdCardTraits = setFromList [AncientOne, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdUnique = True
    }

swarmOfRats :: CardDef
swarmOfRats = (enemy "01159" "Swarm of Rats" Rats 3)
  { cdCardTraits = setFromList [Creature]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

ghoulMinion :: CardDef
ghoulMinion = (enemy "01160" "Ghoul Minion" Ghouls 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

ravenousGhoul :: CardDef
ravenousGhoul = (enemy "01161" "Ravenous Ghoul" Ghouls 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyte :: CardDef
acolyte = (enemy "01169" "Acolyte" DarkCult 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  }

wizardOfTheOrder :: CardDef
wizardOfTheOrder = (enemy "01170" "Wizard of the Order" DarkCult 1)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

huntingNightgaunt :: CardDef
huntingNightgaunt = (enemy "01172" "Hunting Nightgaunt" Nightgaunts 2)
  { cdCardTraits = setFromList [Monster, Nightgaunt]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

screechingByakhee :: CardDef
screechingByakhee = (enemy "01175" "Screeching Byakhee" AgentsOfHastur 2)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

yithianObserver :: CardDef
yithianObserver = (enemy "01177" "Yithian Observer" AgentsOfYogSothoth 2)
  { cdCardTraits = setFromList [Monster, Yithian]
  , cdVictoryPoints = Just 1
  }

relentlessDarkYoung :: CardDef
relentlessDarkYoung =
  (enemy "01179" "Relentless Dark Young" AgentsOfShubNiggurath 1)
    { cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

goatSpawn :: CardDef
goatSpawn = (enemy "01180" "Goat Spawn" AgentsOfShubNiggurath 3)
  { cdCardTraits = setFromList [Humanoid, Monster]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

youngDeepOne :: CardDef
youngDeepOne = (enemy "01181" "Young Deep One" AgentsOfCthulhu 2)
  { cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

theExperiment :: CardDef
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

cloverClubPitBoss :: CardDef
cloverClubPitBoss = (enemy "02078" "Clover Club Pit Boss" TheHouseAlwaysWins 1)
  { cdCardTraits = setFromList [Humanoid, Criminal, Elite]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

thrall :: CardDef
thrall = (enemy "02086" "Thrall" BishopsThralls 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

wizardOfYogSothoth :: CardDef
wizardOfYogSothoth = (enemy "02087" "Wizard of Yog-Sothoth" BishopsThralls 1)
  { cdCardTraits = setFromList [Humanoid, Sorcerer]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

whippoorwill :: CardDef
whippoorwill = (enemy "02090" "Whippoorwill" Whippoorwills 3)
  { cdCardTraits = setFromList [Creature]
  , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
  }

avianThrall :: CardDef
avianThrall = (enemy "02094" "Avian Thrall" BeastThralls 2)
  { cdCardTraits = setFromList [Creature, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

lupineThrall :: CardDef
lupineThrall = (enemy "02095" "Lupine Thrall" BeastThralls 2)
  { cdCardTraits = setFromList [Creature, Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

oBannionsThug :: CardDef
oBannionsThug = (enemy "02097" "O'Bannion's Thug" NaomisCrew 2)
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  }

mobster :: CardDef
mobster = (enemy "02098" "Mobster" NaomisCrew 2)
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

conglomerationOfSpheres :: CardDef
conglomerationOfSpheres =
  (enemy "02103" "Conglomeration of Spheres" HideousAbominations 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardDef
servantOfTheLurker =
  (enemy "02104" "Servant of the Lurker" HideousAbominations 1)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

huntingHorror :: CardDef
huntingHorror = (enemy
                  "02141"
                  ("Hunting Horror" <:> "Spawned from the Void")
                  TheMiskatonicMuseum
                  1
                )
  { cdCardTraits = setFromList [Monster, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

grapplingHorror :: CardDef
grapplingHorror = (enemy "02182" "Grappling Horror" TheEssexCountyExpress 2)
  { cdCardTraits = setFromList [Monster, Abomination]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

emergentMonstrosity :: CardDef
emergentMonstrosity =
  (enemy "02183" "Emergent Monstrosity" TheEssexCountyExpress 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdVictoryPoints = Just 1
    }

silasBishop :: CardDef
silasBishop =
  (enemy "02216" ("Silas Bishop" <:> "Infused With Evil") BloodOnTheAltar 1)
    { cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

servantOfManyMouths :: CardDef
servantOfManyMouths = (enemy "02224" "Servant of Many Mouths" BloodOnTheAltar 3
                      )
  { cdCardTraits = singleton Humanoid
  , cdKeywords = singleton Keyword.Retaliate
  }

broodOfYogSothoth :: CardDef
broodOfYogSothoth =
  (enemy "02255" "Brood of Yog-Sothoth" UndimensionedAndUnseen 5)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

sethBishop :: CardDef
sethBishop =
  (enemy "02293" ("Seth Bishop" <:> "Sorcerer of Dunwich") WhereDoomAwaits 1)
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

devoteeOfTheKey :: CardDef
devoteeOfTheKey = (enemy "02294" "Devotee of the Key" WhereDoomAwaits 2)
  { cdCardTraits = setFromList [Humanoid, Sorcerer]
  }

crazedShoggoth :: CardDef
crazedShoggoth = (enemy "02295" "Crazed Shoggoth" WhereDoomAwaits 1)
  { cdCardTraits = setFromList [Monster, Shoggoth]
  , cdVictoryPoints = Just 1
  }

yogSothoth :: CardDef
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

interstellarTraveler :: CardDef
interstellarTraveler =
  (enemy "02329" "Interstellar Traveler" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardDef
yithianStarseeker = (enemy "02330" "Yithian Starseeker" LostInTimeAndSpace 2)
  { cdCardTraits = setFromList [Monster, Yithian]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

graveyardGhouls :: CardDef
graveyardGhouls = (weakness "03017" "Graveyard Ghouls")
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

theThingThatFollows :: CardDef
theThingThatFollows = (basicWeakness "03042" "The Thing That Follows")
  { cdCardTraits = setFromList [Monster, Curse]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdUnique = True
  }

theManInThePallidMask :: CardDef
theManInThePallidMask = (weakness "03059" "The Man in the Pallid Mask")
  { cdCardTraits = setFromList [Humanoid, Elite]
  , cdKeywords = setFromList [Keyword.Aloof]
  , cdUnique = True
  }

royalEmissary :: CardDef
royalEmissary =
  (enemy "03060" ("Royal Emissary" <:> "Messenger from Aldebaran") CurtainCall 1
    )
    { cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList
      [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

constanceDumaine :: CardDef
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

jordanPerry :: CardDef
jordanPerry =
  (enemy "03066b" ("Jordan Perry" <:> "An Imposing Presence") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

ishimaruHaruko :: CardDef
ishimaruHaruko =
  (enemy "03067b" ("Ishimaru Haruku" <:> "Just Skin and Bones") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

sebastienMoreau :: CardDef
sebastienMoreau =
  (enemy "03068b" ("Sebastien Moreau" <:> "Savage Hysteria") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

ashleighClarke :: CardDef
ashleighClarke =
  (enemy "03069b" ("Ashleigh Clarke" <:> "Songs Die Unheard") TheLastKing 1)
    { cdCardTraits = setFromList [Monster, Lunatic, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdUnique = True
    , cdDoubleSided = True
    , cdVictoryPoints = Just 0
    }

dianneDevine :: CardDef
dianneDevine =
  (enemy "03081" ("Dianne Devine" <:> "Mercurial and Mischevious") TheLastKing 1
    )
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = singleton Keyword.Aloof
    , cdUnique = True
    }

swiftByakhee :: CardDef
swiftByakhee = (enemy "03086" "Swift Byakhee" EncounterSet.Byakhee 2)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

beastOfAldebaran :: CardDef
beastOfAldebaran = (enemy "03088" "Beast of Aldebaran" InhabitantsOfCarcosa 1)
  { cdCardTraits = setFromList [Monster, Elite]
  , cdKeywords = singleton Keyword.Massive
  , cdVictoryPoints = Just 1
  }

spawnOfHali :: CardDef
spawnOfHali = (enemy "03089" "Spawn of Hali" InhabitantsOfCarcosa 2)
  { cdCardTraits = singleton Monster
  , cdKeywords = singleton Keyword.Retaliate
  }

poltergeist :: CardDef
poltergeist = (enemy "03093" "Poltergeist" Hauntings 2)
  { cdCardTraits = setFromList [Monster, Geist]
  }

maniac :: CardDef
maniac = (enemy "03095" "Maniac" HastursGift 2)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

youngPsychopath :: CardDef
youngPsychopath = (enemy "03096" "Young Psychopath" HastursGift 2)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

fanatic :: CardDef
fanatic = (enemy "03098" "Fanatic" CultOfTheYellowSign 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  }

agentOfTheKing :: CardDef
agentOfTheKing = (enemy "03099" "Agent of the King" CultOfTheYellowSign 1)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = singleton Keyword.Hunter
  , cdVictoryPoints = Just 1
  }

roachSwarm :: CardDef
roachSwarm = (enemy "03103" "Roach Swarm" DecayAndFilth 2)
  { cdCardTraits = singleton Creature
  }

possessedOathspeaker :: CardDef
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

seekerOfCarcosa :: CardDef
seekerOfCarcosa = (enemy "03144" "Seeker of Carcosa" EchoesOfThePast 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = singleton Keyword.Aloof
  }

danielChesterfield :: CardDef
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

asylumGorger :: CardDef
asylumGorger = (enemy "03183" "Asylum Gorger" TheUnspeakableOath 2)
  { cdCardTraits = setFromList [Monster, Abomination]
  , cdKeywords = singleton Keyword.Hunter
  }

madPatient :: CardDef
madPatient = (enemy "03184" "Mad Patient" TheUnspeakableOath 3)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

theOrganistHopelessIDefiedHim :: CardDef
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

theOrganistDrapedInMystery :: CardDef
theOrganistDrapedInMystery =
  (enemy "03221b" ("The Organist" <:> "Draped in Mystery") APhantomOfTruth 1)
    { cdCardTraits = setFromList [Humanoid, Avatar, Elite]
    , cdKeywords = singleton Keyword.Aloof
    , cdUnique = True
    , cdDoubleSided = True
    }

stealthyByakhee :: CardDef
stealthyByakhee = (enemy "03222" "Stealthy Byakhee" APhantomOfTruth 2)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = singleton Keyword.Hunter
  }

specterOfDeath :: CardDef
specterOfDeath =
  (enemy "03241b" ("Specter of Death" <:> "A Force From Beyond") ThePallidMask 1)
    { cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    }

catacombsDocent :: CardDef
catacombsDocent = (enemy "03258" "Catacombs Docent" ThePallidMask 3)
  { cdCardTraits = setFromList [Humanoid, Lunatic]
  }

corpseDweller :: CardDef
corpseDweller = (enemy "03259" "Corpse Dweller" ThePallidMask 3)
  { cdCardTraits = singleton Monster
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

tidalTerror :: CardDef
tidalTerror = (enemy "03300" "Tidal Terror" BlackStarsRise 2)
  { cdCardTraits = singleton Monster
  , cdKeywords = singleton Keyword.Hunter
  }

riftSeeker :: CardDef
riftSeeker = (enemy "03301" "Rift Seeker" BlackStarsRise 2)
  { cdCardTraits = setFromList [Monster, Byakhee, Cultist]
  }

hasturTheKingInYellow :: CardDef
hasturTheKingInYellow = (enemy "03332" ("Hastur" <:> "The King in Yellow") DimCarcosa 1)
  { cdCardTraits = setFromList [AncientOne, Elite]
  , cdUnique = True
  }

hasturLordOfCarcosa :: CardDef
hasturLordOfCarcosa = (enemy "03333" ("Hastur" <:> "Lord of Carcosa") DimCarcosa 1)
  { cdCardTraits = setFromList [AncientOne, Elite]
  , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
  , cdUnique = True
  }

hasturTheTatteredKing :: CardDef
hasturTheTatteredKing = (enemy "03334" ("Hastur" <:> "The Tattered King") DimCarcosa 1)
  { cdCardTraits = setFromList [AncientOne, Elite]
  , cdKeywords = singleton Keyword.Hunter
  , cdUnique = True
  }

creatureOutOfDemhe :: CardDef
creatureOutOfDemhe = (enemy "03335" "Creature Out of Demhe" DimCarcosa 1)
  { cdCardTraits = singleton Monster
  , cdKeywords = singleton Keyword.Massive
  }

wingedOne :: CardDef
wingedOne = (enemy "03336" "Winged One" DimCarcosa 1)
  { cdCardTraits = setFromList [Monster, Byakhee]
  , cdKeywords = singleton Keyword.Retaliate
  }

serpentsOfYig :: CardDef
serpentsOfYig = (weakness "04014" "Serpents of Yig")
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdKeywords = singleton Keyword.Hunter
  , cdRevelation = True
  }

ichtaca :: CardDef
ichtaca = (enemy "04052" ("Ichtaca" <:> "Keeper of the Eztli") TheUntamedWilds 1)
  { cdCardTraits = setFromList [Humanoid, Eztli, Elite]
  , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
  , cdVictoryPoints = Just 1
  }

pitViper :: CardDef
pitViper = (enemy "04078" "Pit Viper" Serpents 3)
  { cdCardTraits = setFromList [Creature, Serpent]
  , cdVengeancePoints = Just 1
  }

boaConstrictor :: CardDef
boaConstrictor = (enemy "04079" "Boa Constrictor" Serpents 1)
  { cdCardTraits = setFromList [Creature, Serpent]
  , cdKeywords = singleton Keyword.Hunter
  , cdVengeancePoints = Just 2
  }

broodOfYig :: CardDef
broodOfYig = (enemy "04083" "Brood of Yig" AgentsOfYig 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdKeywords = singleton Keyword.Hunter
  }

serpentFromYoth :: CardDef
serpentFromYoth = (enemy "04084" "Serpent from Yoth" AgentsOfYig 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Serpent]
  , cdVictoryPoints = Just 1
  }

eztliGuardian :: CardDef
eztliGuardian = (enemy "04086" "Eztli Guardian" GuardiansOfTime 2)
  { cdCardTraits = setFromList [Humanoid, Eztli]
  , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
  }

corpseHungryGhoul :: CardDef
corpseHungryGhoul = (enemy "50022" "Corpse-Hungry Ghoul" ReturnToTheGathering 1
                    )
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdVictoryPoints = Just 1
  }

ghoulFromTheDepths :: CardDef
ghoulFromTheDepths =
  (enemy "50023" "Ghoul from the Depths" ReturnToTheGathering 1)
    { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    }

narogath :: CardDef
narogath =
  (enemy "50026b" ("Narôgath" <:> "The Charnel Lord") ReturnToTheMidnightMasks 1
    )
    { cdCardTraits = setFromList [Humanoid, Monster, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

graveEater :: CardDef
graveEater = (enemy "50038" "Grave-Eater" GhoulsOfUmordhoth 3)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyteOfUmordhoth :: CardDef
acolyteOfUmordhoth = (enemy "50039" "Acolyte of Umôrdhoth" GhoulsOfUmordhoth 1)
  { cdCardTraits = setFromList [Humanoid, Monster, Ghoul]
  }

discipleOfTheDevourer :: CardDef
discipleOfTheDevourer =
  (enemy "50041" "Disciple of the Devourer" TheDevourersCult 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    }

corpseTaker :: CardDef
corpseTaker = (enemy "50042" "Corpse-Taker" TheDevourersCult 1)
  { cdCardTraits = setFromList [Monster, Servitor, Cultist]
  }

jeremiahPierce :: CardDef
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

billyCooper :: CardDef
billyCooper =
  (enemy "50045" ("Billy Cooper" <:> "The Crooked Cop") ReturnCultOfUmordhoth 1)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

almaHill :: CardDef
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

tommyMalloy :: CardDef
tommyMalloy = (weakness "60103" "Tommy Malloy")
  { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
  , cdKeywords = setFromList [Keyword.Hunter]
  , cdUnique = True
  }

bogGator :: CardDef
bogGator = (enemy "81022" "Bog Gator" TheBayou 2)
  { cdCardTraits = setFromList [Creature]
  }

swampLeech :: CardDef
swampLeech = (enemy "81023" "Swamp Leech" TheBayou 3)
  { cdCardTraits = setFromList [Creature]
  }

theRougarou :: CardDef
theRougarou =
  (enemy "81028" ("The Rougarou" <:> "Cursed Soul") CurseOfTheRougarou 1)
    { cdCardTraits = setFromList [Monster, Creature, Elite]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    , cdUnique = True
    }

slimeCoveredDhole :: CardDef
slimeCoveredDhole = (enemy "81031" "Slime-Covered Dhole" CurseOfTheRougarou 2)
  { cdCardTraits = setFromList [Monster, Dhole]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

marshGug :: CardDef
marshGug = (enemy "81032" "Marsh Gug" CurseOfTheRougarou 2)
  { cdCardTraits = setFromList [Monster, Gug]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

darkYoungHost :: CardDef
darkYoungHost = (enemy "81033" "Dark Young Host" CurseOfTheRougarou 1)
  { cdCardTraits = setFromList [Monster, DarkYoung]
  , cdVictoryPoints = Just 1
  }

balefulReveler :: CardDef
balefulReveler =
  (enemy "82002b" ("Baleful Reveler" <:> "Spreading Chaos") CarnevaleOfHorrors 1
    )
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

donLagorio :: CardDef
donLagorio =
  (enemy "82017" ("Don Lagorio" <:> "Secret Servant") CarnevaleOfHorrors 1)
    { cdCardTraits = setFromList [Humanoid, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

elisabettaMagro :: CardDef
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

salvatoreNeri :: CardDef
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

savioCorvi :: CardDef
savioCorvi =
  (enemy "82020" ("Savio Corvi" <:> "Dark Lurker") CarnevaleOfHorrors 1)
    { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

cnidathqua :: CardDef
cnidathqua =
  (enemy "82027" ("Cnidathqua" <:> "The Many-armed Beast") CarnevaleOfHorrors 1)
    { cdCardTraits = setFromList [Monster, AncientOne, Elite]
    , cdUnique = True
    }

poleman :: CardDef
poleman = (enemy "82028" "Poleman" CarnevaleOfHorrors 2)
  { cdCardTraits = setFromList [Monster, DeepOne]
  , cdKeywords = setFromList [Keyword.Hunter]
  }

carnevaleSentinel :: CardDef
carnevaleSentinel = (enemy "82029" "Carnevale Sentinel" CarnevaleOfHorrors 3)
  { cdCardTraits = setFromList [Humanoid, Cultist]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }

writhingAppendage :: CardDef
writhingAppendage = (enemy "82030" "Writhing Appendage" CarnevaleOfHorrors 3)
  { cdCardTraits = setFromList [Monster, Tentacle]
  , cdKeywords = setFromList [Keyword.Retaliate]
  }
