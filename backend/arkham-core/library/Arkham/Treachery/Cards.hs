module Arkham.Treachery.Cards where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.CommitRestriction
import Arkham.EncounterSet hiding ( Byakhee, Dunwich, Poison )
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Keyword qualified as Keyword
import Arkham.Name
import Arkham.Trait

baseTreachery
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef k
baseTreachery cardCode name mEncounterSet isWeakness = CardDef
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
  , cdRevelation = True
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

weakness :: CardCode -> Name -> CardDef 'TreacheryType
weakness cardCode name = baseTreachery cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef 'TreacheryType
basicWeakness cardCode name =
  baseTreachery cardCode name Nothing (Just BasicWeakness)

treachery :: CardCode -> Name -> EncounterSet -> Int -> CardDef 'TreacheryType
treachery cardCode name encounterSet encounterSetQuantity = baseTreachery
  cardCode
  name
  (Just (encounterSet, encounterSetQuantity))
  Nothing

allTreacheryCards :: HashMap CardCode (CardDef 'TreacheryType)
allTreacheryCards = allPlayerTreacheryCards <> allEncounterTreacheryCards

allPlayerTreacheryCards :: HashMap CardCode (CardDef 'TreacheryType)
allPlayerTreacheryCards = mapFromList $ concatMap
  toCardCodePairs
  [ abandonedAndAlone
  , accursedFate
  , acrossSpaceAndTime
  , amnesia
  , angeredSpirits
  , atychiphobia
  , boughtInBlood
  , callOfTheUnknown
  , calledByTheMists
  , caughtRedHanded
  , chronophobia
  , coverUp
  , crisisOfIdentity
  , curseOfTheRougarou
  , doomed
  , drawingTheSign
  , finalRhapsody
  , haunted
  , hospitalDebts
  , hypochondria
  , indebted
  , internalInjury
  , lostSoul
  , obsessive
  , outOfBodyExperience
  , overzealous
  , paranoia
  , poisoned
  , psychosis
  , rationalThought
  , rexsCurse
  , searchingForIzzie
  , selfDestructive
  , shellShock
  , smiteTheWicked
  , starsOfHyades
  , terribleSecret
  , the13thVision
  , theBellTolls
  , theHarbinger
  , thePriceOfFailure
  , thriceDamnedCuriosity
  , voiceOfTheMessenger
  , wrackedByNightmares
  ]

allEncounterTreacheryCards :: HashMap CardCode (CardDef 'TreacheryType)
allEncounterTreacheryCards = mapFrom
  toCardCode
  [ aTearInTime
  , abduction
  , acridMiasma
  , alteredBeast
  , ancestralFear
  , ancientEvils
  , ants
  , arcaneBarrier
  , arousingSuspicions
  , arrowsFromTheTrees
  , attractingAttention
  , bathophobia
  , beastOfTheBayou
  , betweenWorlds
  , beyondTheVeil
  , blackStarsRise
  , brokenRails
  , captiveMind
  , chaosInTheWater
  , childrenOfValusia
  , chillFromBelow
  , clawsOfSteam
  , collapsingReality
  , conspiracyOfBlood
  , corrosion
  , crashingFloods
  , creepingDarkness
  , creepingPoison
  , cruelInterrogations
  , cryptChill
  , curseOfYig
  , cursedLuck
  , cursedSwamp
  , danceOfTheYellowKing
  , deadlyFate
  , deepDark
  , descentIntoMadness
  , dismalCurse
  , dissonantVoices
  , draggedUnder
  , dreamsOfRlyeh
  , eagerForDeath
  , entombed
  , ephemeralExhibits
  , eyesInTheWalls
  , falseLead
  , finalMistake
  , fineDining
  , frozenInFear
  , frozenInFearAPhantomOfTruth
  , giftOfMadnessMisery
  , giftOfMadnessPity
  , graspingHands
  , huntedByByakhee
  , huntedDown
  , huntingShadow
  , illOmen
  , insatiableBloodlust
  , kidnapped
  , ledAstray
  , lightlessShadow
  , lightOfAforgomon
  , lockedDoor
  , lostHumanity
  , lostInTheWilds
  , lostInTime
  , lostInVenice
  , lowOnSupplies
  , markedByTheSign
  , maskOfUmordhoth
  , maskedHorrors
  , massHysteria
  , mesmerize
  , mysteriousChanting
  , noTurningBack
  , nobodysHome
  , obscuringFog
  , offerOfPower
  , onTheProwl
  , onWingsOfDarkness
  , oozeAndFilth
  , overgrowth
  , passageIntoTheVeil
  , pitfall
  , poisonousSpores
  , possessionMurderous
  , possessionTorturous
  , possessionTraitorous
  , psychopompsSong
  , pushedIntoTheBeyond
  , realmOfMadness
  , ripplesOnTheSurface
  , ritesHowled
  , rottingRemains
  , rottingRemainsBloodOnTheAltar
  , ruinAndDestruction
  , serpentsCall
  , serpentsIre
  , shadowSpawned
  , shadowed
  , shatteredAges
  , slitheringBehindYou
  , snakeBite
  , snakescourge
  , somethingInTheDrinks
  , sordidAndSilent
  , spacesBetween
  , spectralMist
  , spiresOfCarcosa
  , spiritsTorment
  , stalkedInTheDark
  , straitjacket
  , strangeSigns
  , terrorFromBeyond
  , theCreaturesTracks
  , theCultsSearch
  , theFinalAct
  , theKingsEdict
  , thePaleMaskBeckons
  , thePitBelow
  , theSecretMustBeKept
  , theShadowBehindYou
  , theYellowSign
  , theZealotsSeal
  , timelineDestabilization
  , torturousChords
  , toughCrowd
  , toweringBeasts
  , twinSuns
  , twistOfFate
  , twistedToHisWill
  , umordhothsHunger
  , umordhothsWrath
  , unhallowedCountry
  , vastExpanse
  , vaultOfEarthlyDemise
  , visionsOfFuturesPast
  , voiceOfTheJungle
  , vortexOfTime
  , wallsClosingIn
  , watchersGaze
  , whispersInYourHeadAnxiety
  , whispersInYourHeadDismay
  , whispersInYourHeadDoubt
  , whispersInYourHeadDread
  , windowToAnotherTime
  , wordsOfPower
  , worldsMerge
  , wormhole
  , wrackedByTime
  , yithianPresence
  ]

coverUp :: CardDef 'TreacheryType
coverUp = (weakness "01007" "Cover Up")
  { cdCardTraits = setFromList [Task]
  , cdAlternateCardCodes = ["01507"]
  }

hospitalDebts :: CardDef 'TreacheryType
hospitalDebts =
  (weakness "01011" "Hospital Debts")
    { cdCardTraits = setFromList [Task]
    , cdAlternateCardCodes = ["01511"]
    }

abandonedAndAlone :: CardDef 'TreacheryType
abandonedAndAlone = (weakness "01015" "Abandoned and Alone")
  { cdCardTraits = setFromList [Madness]
  , cdAlternateCardCodes = ["01515"]
  }

amnesia :: CardDef 'TreacheryType
amnesia = (basicWeakness "01096" "Amnesia")
  { cdCardTraits = setFromList [Madness]
  , cdAlternateCardCodes = ["01596"]
  }

paranoia :: CardDef 'TreacheryType
paranoia = (basicWeakness "01097" "Paranoia")
  { cdCardTraits = setFromList [Madness]
  , cdAlternateCardCodes = ["01597"]
  }

haunted :: CardDef 'TreacheryType
haunted = (basicWeakness "01098" "Haunted")
  { cdCardTraits = setFromList [Curse]
  , cdAlternateCardCodes = ["01598"]
  }

psychosis :: CardDef 'TreacheryType
psychosis = (basicWeakness "01099" "Psychosis")
  { cdCardTraits = setFromList [Madness]
  , cdAlternateCardCodes = ["01599"]
  }

hypochondria :: CardDef 'TreacheryType
hypochondria = (basicWeakness "01100" "Hypochondria")
  { cdCardTraits = setFromList [Madness]
  , cdAlternateCardCodes = ["01600"]
  }

huntingShadow :: CardDef 'TreacheryType
huntingShadow = (treachery "01135" "Hunting Shadow" TheMidnightMasks 3)
  { cdCardTraits = setFromList [Curse]
  , cdKeywords = setFromList [Keyword.Peril]
  }

falseLead :: CardDef 'TreacheryType
falseLead = treachery "01136" "False Lead" TheMidnightMasks 2

umordhothsWrath :: CardDef 'TreacheryType
umordhothsWrath = (treachery "01158" "Umôrdhoth's Wrath" TheDevourerBelow 2)
  { cdCardTraits = setFromList [Curse]
  }

graspingHands :: CardDef 'TreacheryType
graspingHands = (treachery "01162" "Grasping Hands" Ghouls 3)
  { cdCardTraits = setFromList [Hazard]
  }

rottingRemains :: CardDef 'TreacheryType
rottingRemains = (treachery "01163" "Rotting Remains" StrikingFear 3)
  { cdCardTraits = setFromList [Terror]
  }

frozenInFear :: CardDef 'TreacheryType
frozenInFear = (treachery "01164" "Frozen in Fear" StrikingFear 2)
  { cdCardTraits = setFromList [Terror]
  }

dissonantVoices :: CardDef 'TreacheryType
dissonantVoices = (treachery "01165" "Dissonant Voices" StrikingFear 2)
  { cdCardTraits = setFromList [Terror]
  }

ancientEvils :: CardDef 'TreacheryType
ancientEvils = (treachery "01166" "Ancient Evils" AncientEvils 3)
  { cdCardTraits = setFromList [Omen]
  }

cryptChill :: CardDef 'TreacheryType
cryptChill = (treachery "01167" "Crypt Chill" ChillingCold 2)
  { cdCardTraits = setFromList [Hazard]
  }

obscuringFog :: CardDef 'TreacheryType
obscuringFog = (treachery "01168" "Obscuring Fog" ChillingCold 2)
  { cdCardTraits = setFromList [Hazard]
  }

mysteriousChanting :: CardDef 'TreacheryType
mysteriousChanting = (treachery "01171" "Mysterious Chanting" DarkCult 2)
  { cdCardTraits = setFromList [Hex]
  }

onWingsOfDarkness :: CardDef 'TreacheryType
onWingsOfDarkness = treachery "01173" "On Wings of Darkness" Nightgaunts 2

lockedDoor :: CardDef 'TreacheryType
lockedDoor = (treachery "01174" "Locked Door" LockedDoors 2)
  { cdCardTraits = setFromList [Obstacle]
  }

theYellowSign :: CardDef 'TreacheryType
theYellowSign = (treachery "01176" "The Yellow Sign" AgentsOfHastur 2)
  { cdCardTraits = setFromList [Omen]
  }

offerOfPower :: CardDef 'TreacheryType
offerOfPower = (treachery "01178" "Offer of Power" AgentsOfYogSothoth 2)
  { cdCardTraits = setFromList [Pact]
  , cdKeywords = setFromList [Keyword.Peril]
  }

dreamsOfRlyeh :: CardDef 'TreacheryType
dreamsOfRlyeh = (treachery "01182" "Dreams of R'lyeh" AgentsOfCthulhu 2)
  { cdCardTraits = setFromList [Omen]
  }

smiteTheWicked :: CardDef 'TreacheryType
smiteTheWicked =
  (weakness "02007" "Smite the Wicked") { cdCardTraits = setFromList [Task] }

rexsCurse :: CardDef 'TreacheryType
rexsCurse =
  (weakness "02009" "Rex's Curse") { cdCardTraits = setFromList [Curse] }

searchingForIzzie :: CardDef 'TreacheryType
searchingForIzzie =
  (weakness "02011" "Searching for Izzie") { cdCardTraits = setFromList [Task] }

finalRhapsody :: CardDef 'TreacheryType
finalRhapsody =
  (weakness "02013" "Final Rhapsody") { cdCardTraits = setFromList [Endtimes] }

wrackedByNightmares :: CardDef 'TreacheryType
wrackedByNightmares = (weakness "02015" "Wracked by Nightmares")
  { cdCardTraits = setFromList [Madness]
  }

indebted :: CardDef 'TreacheryType
indebted = (basicWeakness "02037" "Indebted")
  { cdCardTraits = singleton Flaw
  , cdPermanent = True
  }

internalInjury :: CardDef 'TreacheryType
internalInjury =
  (basicWeakness "02038" "Internal Injury") { cdCardTraits = singleton Injury }

chronophobia :: CardDef 'TreacheryType
chronophobia =
  (basicWeakness "02039" "Chronophobia") { cdCardTraits = singleton Madness }

somethingInTheDrinks :: CardDef 'TreacheryType
somethingInTheDrinks =
  (treachery "02081" "Something in the Drinks" TheHouseAlwaysWins 2)
    { cdCardTraits = setFromList [Poison, Illicit]
    , cdKeywords = setFromList [Keyword.Surge]
    }

arousingSuspicions :: CardDef 'TreacheryType
arousingSuspicions =
  treachery "02082" "Arousing Suspicions" TheHouseAlwaysWins 2

visionsOfFuturesPast :: CardDef 'TreacheryType
visionsOfFuturesPast = (treachery "02083" "Visions of Futures Past" Sorcery 3)
  { cdCardTraits = setFromList [Hex]
  }

beyondTheVeil :: CardDef 'TreacheryType
beyondTheVeil = (treachery "02084" "Beyond the Veil" Sorcery 3)
  { cdCardTraits = setFromList [Hex]
  , cdKeywords = setFromList [Keyword.Surge]
  }

lightOfAforgomon :: CardDef 'TreacheryType
lightOfAforgomon = (treachery "02085" "Light of Aforgomon" BishopsThralls 2)
  { cdCardTraits = setFromList [Pact, Power]
  , cdKeywords = setFromList [Keyword.Peril]
  }

unhallowedCountry :: CardDef 'TreacheryType
unhallowedCountry =
  (treachery "02088" "Unhallowed Country" EncounterSet.Dunwich 2)
    { cdCardTraits = setFromList [Terror]
    }

sordidAndSilent :: CardDef 'TreacheryType
sordidAndSilent = (treachery "02089" "Sordid and Silent" EncounterSet.Dunwich 2
                  )
  { cdCardTraits = setFromList [Terror]
  }

eagerForDeath :: CardDef 'TreacheryType
eagerForDeath = (treachery "02091" "Eager for Death" Whippoorwills 2)
  { cdCardTraits = setFromList [Omen]
  }

cursedLuck :: CardDef 'TreacheryType
cursedLuck = (treachery "02092" "Cursed Luck" BadLuck 3)
  { cdCardTraits = setFromList [Omen]
  }

twistOfFate :: CardDef 'TreacheryType
twistOfFate = (treachery "02093" "Twist of Fate" BadLuck 3)
  { cdCardTraits = setFromList [Omen]
  }

alteredBeast :: CardDef 'TreacheryType
alteredBeast = (treachery "02096" "Altered Beast" BeastThralls 2)
  { cdCardTraits = setFromList [Power]
  }

huntedDown :: CardDef 'TreacheryType
huntedDown = (treachery "02099" "Hunted Down" NaomisCrew 2)
  { cdCardTraits = setFromList [Tactic]
  }

pushedIntoTheBeyond :: CardDef 'TreacheryType
pushedIntoTheBeyond = (treachery "02100" "Pushed into the Beyond" TheBeyond 2)
  { cdCardTraits = setFromList [Hex]
  }

terrorFromBeyond :: CardDef 'TreacheryType
terrorFromBeyond = (treachery "02101" "Terror from Beyond" TheBeyond 2)
  { cdCardTraits = setFromList [Hex, Terror]
  , cdKeywords = setFromList [Keyword.Peril]
  }

arcaneBarrier :: CardDef 'TreacheryType
arcaneBarrier = (treachery "02102" "Arcane Barrier" TheBeyond 2)
  { cdCardTraits = setFromList [Hex, Obstacle]
  }

shadowSpawned :: CardDef 'TreacheryType
shadowSpawned = (treachery "02142" "Shadow-spawned" TheMiskatonicMuseum 1)
  { cdCardTraits = singleton Power
  }

stalkedInTheDark :: CardDef 'TreacheryType
stalkedInTheDark =
  (treachery "02143" "Stalked in the Dark" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Tactic
    }

passageIntoTheVeil :: CardDef 'TreacheryType
passageIntoTheVeil =
  (treachery "02144" "Passage into the Veil" TheMiskatonicMuseum 3)
    { cdCardTraits = singleton Power
    }

ephemeralExhibits :: CardDef 'TreacheryType
ephemeralExhibits =
  (treachery "02145" "Ephemeral Exhibits" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Terror
    }

slitheringBehindYou :: CardDef 'TreacheryType
slitheringBehindYou =
  treachery "02146" "Slithering Behind You" TheMiskatonicMuseum 2

acrossSpaceAndTime :: CardDef 'TreacheryType
acrossSpaceAndTime = (weakness "02178" "Across Space and Time")
  { cdCardTraits = setFromList [Madness]
  , cdEncounterSet = Just TheEssexCountyExpress
  , cdEncounterSetQuantity = Just 4
  }

clawsOfSteam :: CardDef 'TreacheryType
clawsOfSteam = (treachery "02180" "Claws of Steam" TheEssexCountyExpress 3)
  { cdCardTraits = singleton Power
  }

brokenRails :: CardDef 'TreacheryType
brokenRails = (treachery "02181" "Broken Rails" TheEssexCountyExpress 3)
  { cdCardTraits = singleton Hazard
  }

kidnapped :: CardDef 'TreacheryType
kidnapped = treachery "02220" "Kidnapped!" BloodOnTheAltar 3

psychopompsSong :: CardDef 'TreacheryType
psychopompsSong = (treachery "02221" "Psychopomp's Song" BloodOnTheAltar 2)
  { cdCardTraits = singleton Omen
  , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
  }

strangeSigns :: CardDef 'TreacheryType
strangeSigns = (treachery "02222" "Strange Signs" BloodOnTheAltar 2)
  { cdCardTraits = singleton Omen
  }

rottingRemainsBloodOnTheAltar :: CardDef 'TreacheryType
rottingRemainsBloodOnTheAltar =
  (treachery "02223" "Rotting Remains" BloodOnTheAltar 3)
    { cdCardTraits = singleton Terror
    }

toweringBeasts :: CardDef 'TreacheryType
toweringBeasts = (treachery "02256" "Towering Beasts" UndimensionedAndUnseen 4)
  { cdKeywords = singleton Keyword.Peril
  }

ruinAndDestruction :: CardDef 'TreacheryType
ruinAndDestruction =
  (treachery "02257" "Ruin and Destruction" UndimensionedAndUnseen 3)
    { cdCardTraits = singleton Hazard
    }

attractingAttention :: CardDef 'TreacheryType
attractingAttention =
  (treachery "02258" "Attracting Attention" UndimensionedAndUnseen 2)
    { cdKeywords = singleton Keyword.Surge
    }

theCreaturesTracks :: CardDef 'TreacheryType
theCreaturesTracks =
  (treachery "02259" "The Creatures' Tracks" UndimensionedAndUnseen 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

ritesHowled :: CardDef 'TreacheryType
ritesHowled = (treachery "02296" "Rites Howled" WhereDoomAwaits 3)
  { cdCardTraits = singleton Hex
  }

spacesBetween :: CardDef 'TreacheryType
spacesBetween = (treachery "02297" "Spaces Between" WhereDoomAwaits 3)
  { cdCardTraits = setFromList [Hex, Hazard]
  }

vortexOfTime :: CardDef 'TreacheryType
vortexOfTime = (treachery "02298" "Vortex of Time" WhereDoomAwaits 3)
  { cdCardTraits = setFromList [Hex, Hazard]
  }

collapsingReality :: CardDef 'TreacheryType
collapsingReality =
  (treachery "02331" "Collapsing Reality" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Hazard]
    }

wormhole :: CardDef 'TreacheryType
wormhole = (treachery "02332" "Wormhole" LostInTimeAndSpace 2)
  { cdCardTraits = setFromList [Hazard]
  }

vastExpanse :: CardDef 'TreacheryType
vastExpanse = (treachery "02333" "Vast Expanse" LostInTimeAndSpace 3)
  { cdCardTraits = setFromList [Terror]
  }

shellShock :: CardDef 'TreacheryType
shellShock =
  (weakness "03008" "Shell Shock") { cdCardTraits = setFromList [Flaw] }

starsOfHyades :: CardDef 'TreacheryType
starsOfHyades =
  (weakness "03013" "Stars of Hyades") { cdCardTraits = setFromList [Curse] }

angeredSpirits :: CardDef 'TreacheryType
angeredSpirits =
  (weakness "03015" "Angered Spirits") { cdCardTraits = singleton Task }

crisisOfIdentity :: CardDef 'TreacheryType
crisisOfIdentity =
  (weakness "03019" "Crisis of Identity") { cdCardTraits = singleton Madness }

overzealous :: CardDef 'TreacheryType
overzealous =
  (basicWeakness "03040" "Overzealous") { cdCardTraits = singleton Flaw }

drawingTheSign :: CardDef 'TreacheryType
drawingTheSign = (basicWeakness "03041" "Drawing the Sign")
  { cdCardTraits = setFromList [Pact, Madness]
  }

fineDining :: CardDef 'TreacheryType
fineDining = (treachery "03082" "Fine Dining" TheLastKing 2)
  { cdCardTraits = singleton Terror
  , cdKeywords = singleton Keyword.Peril
  }

toughCrowd :: CardDef 'TreacheryType
toughCrowd = (treachery "03083" "Tough Crowd" TheLastKing 2)
  { cdCardTraits = singleton Hazard
  }

whispersInYourHeadDismay :: CardDef 'TreacheryType
whispersInYourHeadDismay =
  (treachery "03084a" "Whispers in Your Head (Dismay)" Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    }

whispersInYourHeadDread :: CardDef 'TreacheryType
whispersInYourHeadDread =
  (treachery "03084b" "Whispers in Your Head (Dread)" Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    }

whispersInYourHeadAnxiety :: CardDef 'TreacheryType
whispersInYourHeadAnxiety =
  (treachery "03084c" "Whispers in Your Head (Anxiety)" Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    }

whispersInYourHeadDoubt :: CardDef 'TreacheryType
whispersInYourHeadDoubt =
  (treachery "03084d" "Whispers in Your Head (Doubt)" Delusions 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    }

descentIntoMadness :: CardDef 'TreacheryType
descentIntoMadness = (treachery "03085" "Descent into Madness" Delusions 2)
  { cdCardTraits = singleton Terror
  , cdKeywords = singleton Keyword.Surge
  }

huntedByByakhee :: CardDef 'TreacheryType
huntedByByakhee = (treachery "03087" "Hunted by Byakhee" EncounterSet.Byakhee 2
                  )
  { cdCardTraits = singleton Pact
  }

blackStarsRise :: CardDef 'TreacheryType
blackStarsRise = (treachery "03090" "Black Stars Rise" EvilPortents 2)
  { cdCardTraits = singleton Omen
  }

spiresOfCarcosa :: CardDef 'TreacheryType
spiresOfCarcosa = (treachery "03091" "Spires of Carcosa" EvilPortents 2)
  { cdCardTraits = singleton Omen
  }

twistedToHisWill :: CardDef 'TreacheryType
twistedToHisWill = (treachery "03092" "Twisted to His Will" EvilPortents 2)
  { cdCardTraits = singleton Pact
  }

spiritsTorment :: CardDef 'TreacheryType
spiritsTorment = (treachery "03094" "Spirit's Torment" Hauntings 2)
  { cdCardTraits = setFromList [Curse, Geist]
  }

danceOfTheYellowKing :: CardDef 'TreacheryType
danceOfTheYellowKing =
  (treachery "03097" "Dance of the Yellow King" HastursGift 2)
    { cdCardTraits = singleton Pact
    }

theKingsEdict :: CardDef 'TreacheryType
theKingsEdict = (treachery "03100" "The King's Edict" CultOfTheYellowSign 2)
  { cdCardTraits = singleton Pact
  }

oozeAndFilth :: CardDef 'TreacheryType
oozeAndFilth = (treachery "03101" "Ooze and Filth" DecayAndFilth 2)
  { cdCardTraits = singleton Hazard
  }

corrosion :: CardDef 'TreacheryType
corrosion = (treachery "03102" "Corrosion" DecayAndFilth 2)
  { cdCardTraits = singleton Hazard
  }

markedByTheSign :: CardDef 'TreacheryType
markedByTheSign = (treachery "03104" "Marked by the Sign" TheStranger 2)
  { cdCardTraits = singleton Pact
  , cdKeywords = singleton Keyword.Peril
  }

thePaleMaskBeckons :: CardDef 'TreacheryType
thePaleMaskBeckons = (treachery "03105" "The Pale Mask Beckons" TheStranger 1)
  { cdCardTraits = setFromList [Omen, Pact]
  }

ledAstray :: CardDef 'TreacheryType
ledAstray = (treachery "03145" "Led Astray" EchoesOfThePast 3)
  { cdCardTraits = singleton Scheme
  , cdKeywords = singleton Keyword.Peril
  }

theCultsSearch :: CardDef 'TreacheryType
theCultsSearch = (treachery "03146" "The Cult's Search" EchoesOfThePast 2)
  { cdCardTraits = singleton Scheme
  }

straitjacket :: CardDef 'TreacheryType
straitjacket = (treachery "03185" "Straitjacket" TheUnspeakableOath 2)
  { cdCardTraits = setFromList [Item, Clothing]
  }

giftOfMadnessPity :: CardDef 'TreacheryType
giftOfMadnessPity =
  (treachery "03186" ("Gift of Madness" <:> "Pity") TheUnspeakableOath 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    }

giftOfMadnessMisery :: CardDef 'TreacheryType
giftOfMadnessMisery =
  (treachery "03187" ("Gift of Madness" <:> "Misery") TheUnspeakableOath 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    }

wallsClosingIn :: CardDef 'TreacheryType
wallsClosingIn = (treachery "03188" "Walls Closing In" TheUnspeakableOath 3)
  { cdCardTraits = singleton Terror
  }

twinSuns :: CardDef 'TreacheryType
twinSuns = (treachery "03223" "Twin Suns" APhantomOfTruth 2)
  { cdCardTraits = singleton Omen
  }

deadlyFate :: CardDef 'TreacheryType
deadlyFate = (treachery "03224" "Deadly Fate" APhantomOfTruth 3)
  { cdCardTraits = singleton Omen
  }

torturousChords :: CardDef 'TreacheryType
torturousChords = (treachery "03225" "Torturous Chords" APhantomOfTruth 3)
  { cdCardTraits = setFromList [Hex, Terror]
  }

frozenInFearAPhantomOfTruth :: CardDef 'TreacheryType
frozenInFearAPhantomOfTruth =
  (treachery "03226" "Frozen in Fear" APhantomOfTruth 2)
    { cdCardTraits = singleton Terror
    }

lostSoul :: CardDef 'TreacheryType
lostSoul = (weakness "03227" "Lost Soul")
  { cdCardTraits = setFromList [Madness, Pact]
  , cdEncounterSet = Just APhantomOfTruth
  , cdEncounterSetQuantity = Just 4
  }

eyesInTheWalls :: CardDef 'TreacheryType
eyesInTheWalls = (treachery "03260" "Eyes in the Walls" ThePallidMask 3)
  { cdCardTraits = singleton Terror
  }

theShadowBehindYou :: CardDef 'TreacheryType
theShadowBehindYou = (treachery "03261" "The Shadow Behind You" ThePallidMask 3
                     )
  { cdCardTraits = singleton Terror
  }

thePitBelow :: CardDef 'TreacheryType
thePitBelow = (treachery "03262" "The Pit Below" ThePallidMask 3)
  { cdCardTraits = singleton Hazard
  }

crashingFloods :: CardDef 'TreacheryType
crashingFloods = (treachery "03302" "Crashing Floods" BlackStarsRise 3)
  { cdCardTraits = singleton Omen
  }

worldsMerge :: CardDef 'TreacheryType
worldsMerge = (treachery "03303" "Worlds Merge" BlackStarsRise 3)
  { cdCardTraits = singleton Omen
  }

dismalCurse :: CardDef 'TreacheryType
dismalCurse = (treachery "03337" "Dismal Curse" DimCarcosa 3)
  { cdCardTraits = setFromList [Curse, Terror]
  }

realmOfMadness :: CardDef 'TreacheryType
realmOfMadness = (treachery "03338" "Realm of Madness" DimCarcosa 2)
  { cdCardTraits = singleton Terror
  }

theFinalAct :: CardDef 'TreacheryType
theFinalAct = (treachery "03339" "The Final Act" DimCarcosa 1)
  { cdCardTraits = singleton Terror
  , cdKeywords = setFromList [Keyword.Surge]
  }

possessionTraitorous :: CardDef 'TreacheryType
possessionTraitorous =
  (treachery "03340" ("Possession" <:> "Traitorous") DimCarcosa 1)
    { cdCardTraits = setFromList [Hex, Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdCardInHandEffects = True
    , cdCommitRestrictions = [CommittableTreachery]
    }

possessionTorturous :: CardDef 'TreacheryType
possessionTorturous = (treachery "03341" ("Possession" <:> "Torturous") DimCarcosa 1)
  { cdCardTraits = setFromList [Hex, Terror]
  , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
  , cdCardInHandEffects = True
  }

possessionMurderous :: CardDef 'TreacheryType
possessionMurderous = (treachery "03342" ("Possession" <:> "Murderous") DimCarcosa 1)
  { cdCardTraits = setFromList [Hex, Terror]
  , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
  , cdCardInHandEffects = True
  }

boughtInBlood :: CardDef 'TreacheryType
boughtInBlood =
  (weakness "04007" "Bought in Blood") { cdCardTraits = singleton Flaw }

callOfTheUnknown :: CardDef 'TreacheryType
callOfTheUnknown =
  (weakness "04009" "Call of the Unknown") { cdCardTraits = singleton Task }

caughtRedHanded :: CardDef 'TreacheryType
caughtRedHanded =
  (weakness "04012" "Caught Red-Handed") { cdCardTraits = singleton Blunder }

voiceOfTheMessenger :: CardDef 'TreacheryType
voiceOfTheMessenger = (weakness "04016" "Voice of the Messenger")
  { cdCardTraits = setFromList [Curse, Pact]
  }

thePriceOfFailure :: CardDef 'TreacheryType
thePriceOfFailure =
  (weakness "04039" "The Price of Failure") { cdCardTraits = singleton Pact }

doomed :: CardDef 'TreacheryType
doomed = (basicWeakness "04040" "Doomed") { cdCardTraits = singleton Curse }

accursedFate :: CardDef 'TreacheryType
accursedFate =
  (weakness "04041" "Accursed Fate") { cdCardTraits = singleton Curse }

theBellTolls :: CardDef 'TreacheryType
theBellTolls =
  (weakness "04042" "The Bell Tolls") { cdCardTraits = singleton Curse }

overgrowth :: CardDef 'TreacheryType
overgrowth = (treachery "04076" "Overgrowth" Rainforest 2)
  { cdCardTraits = singleton Obstacle
  }

voiceOfTheJungle :: CardDef 'TreacheryType
voiceOfTheJungle = (treachery "04077" "Voice of the Jungle" Rainforest 2)
  { cdCardTraits = singleton Power
  }

snakeBite :: CardDef 'TreacheryType
snakeBite = (treachery "04080" "Snake Bite" Serpents 3)
  { cdCardTraits = setFromList [Hazard, Poison]
  }

lostInTheWilds :: CardDef 'TreacheryType
lostInTheWilds = (treachery "04081" "Lost in the Wilds" Expedition 3)
  { cdCardTraits = singleton Blunder
  }

lowOnSupplies :: CardDef 'TreacheryType
lowOnSupplies = (treachery "04082" "Low on Supplies" Expedition 2)
  { cdCardTraits = singleton Blunder
  , cdKeywords = singleton Keyword.Peril
  }

curseOfYig :: CardDef 'TreacheryType
curseOfYig = (treachery "04085" "Curse of Yig" AgentsOfYig 2)
  { cdCardTraits = singleton Curse
  }

arrowsFromTheTrees :: CardDef 'TreacheryType
arrowsFromTheTrees =
  (treachery "04087" "Arrows from the Trees" GuardiansOfTime 2)
    { cdCardTraits = singleton Scheme
    }

finalMistake :: CardDef 'TreacheryType
finalMistake =
  (treachery "04088" "Final Mistake" DeadlyTraps 3)
    { cdCardTraits = singleton Trap
    }

entombed :: CardDef 'TreacheryType
entombed =
  (treachery "04089" "Entombed" DeadlyTraps 2)
    { cdCardTraits = singleton Trap
    }

aTearInTime :: CardDef 'TreacheryType
aTearInTime =
  (treachery "04090" "A Tear in Time" TemporalFlux 3)
    { cdCardTraits = singleton Hex
    }

lostInTime :: CardDef 'TreacheryType
lostInTime =
  (treachery "04091" "Lost in Time" TemporalFlux 2)
    { cdCardTraits = singleton Hex
    }

illOmen :: CardDef 'TreacheryType
illOmen =
  (treachery "04092" "Ill Omen" ForgottenRuins 2)
    { cdCardTraits = setFromList [Omen, Terror]
    , cdKeywords = singleton Keyword.Peril
    }

ancestralFear :: CardDef 'TreacheryType
ancestralFear =
  (treachery "04093" "Ancestral Fear" ForgottenRuins 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Surge]
    }

deepDark :: CardDef 'TreacheryType
deepDark =
  (treachery "04094" "Deep Dark" ForgottenRuins 3)
    { cdCardTraits = singleton Hazard
    }

shadowed :: CardDef 'TreacheryType
shadowed =
  (treachery "04096" "Shadowed" PnakoticBrotherhood 2)
    { cdCardTraits = singleton Scheme
    }

wordsOfPower :: CardDef 'TreacheryType
wordsOfPower =
  (treachery "04097" "Words of Power" PnakoticBrotherhood 2)
    { cdCardTraits = singleton Hex
    }

snakescourge :: CardDef 'TreacheryType
snakescourge =
  (treachery "04099" "Snakescourge" YigsVenom 2)
    { cdCardTraits = singleton Curse
    }

serpentsCall :: CardDef 'TreacheryType
serpentsCall =
  (treachery "04100" "Serpent's Call" YigsVenom 1)
    { cdCardTraits = singleton Power
    }

creepingPoison :: CardDef 'TreacheryType
creepingPoison =
  (treachery "04101" "Creeping Poison" EncounterSet.Poison 2)
    { cdCardTraits = singleton Poison
    , cdKeywords = singleton Keyword.Surge
    }

poisoned :: CardDef 'TreacheryType
poisoned =
  (weakness "04102" "Poisoned")
    { cdCardTraits = singleton Poison
    , cdPermanent = True
    , cdEncounterSet = Just EncounterSet.Poison
    , cdEncounterSetQuantity = Just 4
    }

theSecretMustBeKept :: CardDef 'TreacheryType
theSecretMustBeKept =
  (treachery "04144" "The Secret Must Be Kept" EncounterSet.ThreadsOfFate 3)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Peril
    }

nobodysHome :: CardDef 'TreacheryType
nobodysHome =
  (treachery "04145" "Nobody's Home" EncounterSet.ThreadsOfFate 2)
    { cdCardTraits = singleton Mystery
    }

conspiracyOfBlood :: CardDef 'TreacheryType
conspiracyOfBlood =
  (treachery "04146" "Conspiracy of Blood" EncounterSet.ThreadsOfFate 2)
    { cdCardTraits = singleton Hex
    }

windowToAnotherTime :: CardDef 'TreacheryType
windowToAnotherTime =
  (treachery "04189" "Window to Another Time" EncounterSet.TheBoundaryBeyond 3)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Peril
    }

timelineDestabilization :: CardDef 'TreacheryType
timelineDestabilization =
  (treachery "04190" "Timeline Destablization" EncounterSet.TheBoundaryBeyond 3)
    { cdCardTraits = singleton Hex
    }

pitfall :: CardDef 'TreacheryType
pitfall =
  (treachery "04215" "Pitfall" EncounterSet.HeartOfTheElders 3)
    { cdCardTraits = singleton Trap
    , cdKeywords = singleton Keyword.Peril
    }

poisonousSpores :: CardDef 'TreacheryType
poisonousSpores =
  (treachery "04216" "Poisonous Spores" EncounterSet.HeartOfTheElders 3)
    { cdCardTraits = singleton Hazard
    }

ants :: CardDef 'TreacheryType
ants =
  (treachery "04221" "Ants!" EncounterSet.PillarsOfJudgement 3)
    { cdCardTraits = singleton Hazard
    }

noTurningBack :: CardDef 'TreacheryType
noTurningBack =
  (treachery "04228" "No Turning Back" EncounterSet.KnYan 3)
    { cdCardTraits = singleton Hazard
    }

yithianPresence :: CardDef 'TreacheryType
yithianPresence =
  (treachery "04260" "Yithian Presence" EncounterSet.TheCityOfArchives 3)
    { cdCardTraits = setFromList [Power, Terror]
    }

cruelInterrogations :: CardDef 'TreacheryType
cruelInterrogations =
  (treachery "04261" "Cruel Interrogations" EncounterSet.TheCityOfArchives 3)
    { cdCardTraits = setFromList [Injury, Terror]
    }

lostHumanity :: CardDef 'TreacheryType
lostHumanity =
  (treachery "04262" "Lost Humanity" EncounterSet.TheCityOfArchives 2)
    { cdCardTraits = singleton Terror
    }

captiveMind :: CardDef 'TreacheryType
captiveMind =
  (treachery "04263" "Captive Mind" EncounterSet.TheCityOfArchives 2)
    { cdCardTraits = singleton Hex
    }

outOfBodyExperience :: CardDef 'TreacheryType
outOfBodyExperience = (weakness "04264" "Out of Body Experience")
  { cdCardTraits = setFromList [Madness, Paradox]
  , cdEncounterSet = Just TheCityOfArchives
  , cdEncounterSetQuantity = Just 4
  }

childrenOfValusia :: CardDef 'TreacheryType
childrenOfValusia = (treachery "04299" "Children of Valusia" TheDepthsOfYoth 3)
  { cdCardTraits = singleton Scheme
  }

lightlessShadow :: CardDef 'TreacheryType
lightlessShadow = (treachery "04300" "Lightless Shadow" TheDepthsOfYoth 3)
  { cdCardTraits = singleton Terror
  }

bathophobia :: CardDef 'TreacheryType
bathophobia = (treachery "04301" "Bathophobia" TheDepthsOfYoth 3)
  { cdCardTraits = singleton Terror
  }

serpentsIre :: CardDef 'TreacheryType
serpentsIre = (treachery "04302" "Serpent's Ire" TheDepthsOfYoth 2)
  { cdCardTraits = singleton Scheme
  }

shatteredAges :: CardDef 'TreacheryType
shatteredAges = (treachery "04339" "Shattered Ages" ShatteredAeons 2)
  { cdCardTraits = singleton Hex
  }

betweenWorlds :: CardDef 'TreacheryType
betweenWorlds = (treachery "04340" "Between Worlds" ShatteredAeons 2)
  { cdCardTraits = singleton Hex
  }

wrackedByTime :: CardDef 'TreacheryType
wrackedByTime = (treachery "04341" "Wracked by Time" ShatteredAeons 3)
  { cdCardTraits = singleton Hex
  }

creepingDarkness :: CardDef 'TreacheryType
creepingDarkness = (treachery "04342" "Creeping Darkness" ShatteredAeons 2)
  { cdCardTraits = singleton Hex
  }

rationalThought :: CardDef 'TreacheryType
rationalThought = (weakness "05008" "Rational Thought")
  { cdCardTraits = singleton Flaw
  }

terribleSecret :: CardDef 'TreacheryType
terribleSecret = (weakness "05015" "Terrible Secret")
  { cdCardTraits = singleton Madness
  }

the13thVision :: CardDef 'TreacheryType
the13thVision = (basicWeakness "05041" "The 13th Vision")
  { cdCardTraits = singleton Omen
  }

theHarbinger :: CardDef 'TreacheryType
theHarbinger = (weakness "08006" "The Harbinger")
  { cdCardTraits = setFromList [Omen, Endtimes]
  }

theZealotsSeal :: CardDef 'TreacheryType
theZealotsSeal = (treachery "50024" "The Zealot's Seal" ReturnToTheGathering 2)
  { cdCardTraits = setFromList [Hex]
  }

maskedHorrors :: CardDef 'TreacheryType
maskedHorrors = (treachery "50031" "Masked Horrors" ReturnToTheMidnightMasks 2)
  { cdCardTraits = setFromList [Power, Scheme]
  }

vaultOfEarthlyDemise :: CardDef 'TreacheryType
vaultOfEarthlyDemise =
  (treachery "50032b" "Vault of Earthly Demise" ReturnToTheDevourerBelow 1)
    { cdCardTraits = setFromList [Eldritch, Otherworld]
    }

umordhothsHunger :: CardDef 'TreacheryType
umordhothsHunger =
  (treachery "50037" "Umôrdhoth's Hunger" ReturnToTheDevourerBelow 2)
    { cdCardTraits = setFromList [Power]
    }

chillFromBelow :: CardDef 'TreacheryType
chillFromBelow = (treachery "50040" "Chill from Below" GhoulsOfUmordhoth 3)
  { cdCardTraits = setFromList [Hazard]
  }

maskOfUmordhoth :: CardDef 'TreacheryType
maskOfUmordhoth = (treachery "50043" "Mask of Umôrdhoth" TheDevourersCult 2)
  { cdCardTraits = setFromList [Item, Mask]
  }

selfDestructive :: CardDef 'TreacheryType
selfDestructive =
  (weakness "60104" "Self-Destructive") { cdCardTraits = singleton Flaw }

thriceDamnedCuriosity :: CardDef 'TreacheryType
thriceDamnedCuriosity = (weakness "60203" "Thrice-Damned Curiosity")
  { cdCardTraits = singleton Flaw
  }

obsessive :: CardDef 'TreacheryType
obsessive = (weakness "60204" "Obsessive")
  { cdCardTraits = singleton Flaw
  }

calledByTheMists :: CardDef 'TreacheryType
calledByTheMists = (weakness "60503" "Called by the Mists")
  { cdCardTraits = setFromList [Curse]
  }

atychiphobia :: CardDef 'TreacheryType
atychiphobia = (basicWeakness "60504" "Atychiphobia")
  { cdCardTraits = setFromList [Madness]
  }

cursedSwamp :: CardDef 'TreacheryType
cursedSwamp = (treachery "81024" "Cursed Swamp" TheBayou 3)
  { cdCardTraits = setFromList [Hazard]
  }

spectralMist :: CardDef 'TreacheryType
spectralMist = (treachery "81025" "Spectral Mist" TheBayou 3)
  { cdCardTraits = setFromList [Hazard]
  }

draggedUnder :: CardDef 'TreacheryType
draggedUnder = (treachery "81026" "Dragged Under" TheBayou 4)
  { cdCardTraits = setFromList [Hazard]
  }

ripplesOnTheSurface :: CardDef 'TreacheryType
ripplesOnTheSurface = (treachery "81027" "Ripples on the Surface" TheBayou 3)
  { cdCardTraits = setFromList [Terror]
  }

curseOfTheRougarou :: CardDef 'TreacheryType
curseOfTheRougarou = (weakness "81029" "Curse of the Rougarou")
  { cdCardTraits = setFromList [Curse]
  , cdEncounterSet = Just CurseOfTheRougarou
  , cdEncounterSetQuantity = Just 1
  }

onTheProwl :: CardDef 'TreacheryType
onTheProwl = (treachery "81034" "On the Prowl" CurseOfTheRougarou 5)
  { cdKeywords = setFromList [Keyword.Surge]
  }

beastOfTheBayou :: CardDef 'TreacheryType
beastOfTheBayou = treachery "81035" "Beast of the Bayou" CurseOfTheRougarou 2

insatiableBloodlust :: CardDef 'TreacheryType
insatiableBloodlust =
  treachery "81036" "Insatiable Bloodlust" CurseOfTheRougarou 3

massHysteria :: CardDef 'TreacheryType
massHysteria = (treachery "82031" "Mass Hysteria" CarnevaleOfHorrors 3)
  { cdCardTraits = singleton Hazard
  , cdKeywords = singleton Keyword.Peril
  }

lostInVenice :: CardDef 'TreacheryType
lostInVenice = (treachery "82032" "Lost in Venice" CarnevaleOfHorrors 3)
  { cdCardTraits = singleton Blunder
  , cdKeywords = singleton Keyword.Peril
  }

watchersGaze :: CardDef 'TreacheryType
watchersGaze = (treachery "82033" "Watchers' Gaze" CarnevaleOfHorrors 3)
  { cdCardTraits = singleton Terror
  }

chaosInTheWater :: CardDef 'TreacheryType
chaosInTheWater = (treachery "82034" "Chaos in the Water" CarnevaleOfHorrors 3)
  { cdCardTraits = singleton Hazard
  }

mesmerize :: CardDef 'TreacheryType
mesmerize = (treachery "82035" "Mesmerize" CarnevaleOfHorrors 2)
  { cdCardTraits = singleton Hex
  }

abduction :: CardDef 'TreacheryType
abduction = (treachery "82036" "Abduction" CarnevaleOfHorrors 2)
  { cdCardTraits = singleton Scheme
  }

acridMiasma :: CardDef 'TreacheryType
acridMiasma = (treachery "82037" "Acrid Miasma" CarnevaleOfHorrors 2)
  { cdCardTraits = singleton Hazard
  }
