module Arkham.Treachery.Cards where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.ClassSymbol
import Arkham.Types.EncounterSet hiding (Dunwich)
import qualified Arkham.Types.EncounterSet as EncounterSet
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.Trait

baseTreachery
  :: CardCode -> Name -> Maybe (EncounterSet, Int) -> Bool -> CardDef
baseTreachery cardCode name mEncounterSet isWeakness = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdLevel = 0
  , cdCardType = if isWeakness then PlayerTreacheryType else TreacheryType
  , cdWeakness = isWeakness
  , cdClassSymbol = if isWeakness then Just Neutral else Nothing
  , cdSkills = mempty
  , cdCardTraits = mempty
  , cdKeywords = mempty
  , cdFast = False
  , cdFastWindows = mempty
  , cdAction = Nothing
  , cdRevelation = True
  , cdVictoryPoints = Nothing
  , cdPlayRestrictions = mempty
  , cdCommitRestrictions = mempty
  , cdAttackOfOpportunityModifiers = mempty
  , cdPermanent = False
  , cdEncounterSet = fst <$> mEncounterSet
  , cdEncounterSetQuantity = snd <$> mEncounterSet
  , cdUnique = False
  , cdDoubleSided = False
  , cdLimits = []
  }

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseTreachery cardCode name Nothing True

treachery :: CardCode -> Name -> EncounterSet -> Int -> CardDef
treachery cardCode name encounterSet encounterSetQuantity =
  baseTreachery cardCode name (Just (encounterSet, encounterSetQuantity)) False

allPlayerTreacheryCards :: HashMap CardCode CardDef
allPlayerTreacheryCards = mapFromList $ map
  (toCardCode &&& id)
  [ abandonedAndAlone
  , acrossSpaceAndTime
  , amnesia
  , atychiphobia
  , chronophobia
  , coverUp
  , curseOfTheRougarou
  , finalRhapsody
  , haunted
  , hospitalDebts
  , hypochondria
  , indebted
  , internalInjury
  , paranoia
  , psychosis
  , rexsCurse
  , searchingForIzzie
  , smiteTheWicked
  , wrackedByNightmares
  ]

allEncounterTreacheryCards :: HashMap CardCode CardDef
allEncounterTreacheryCards = mapFromList $ map
  (toCardCode &&& id)
  [ placeholderTreachery
  , alteredBeast
  , ancientEvils
  , arcaneBarrier
  , arousingSuspicions
  , attractingAttention
  , beastOfTheBayou
  , beyondTheVeil
  , brokenRails
  , chillFromBelow
  , clawsOfSteam
  , collapsingReality
  , cryptChill
  , cursedLuck
  , cursedSwamp
  , dissonantVoices
  , draggedUnder
  , dreamsOfRlyeh
  , eagerForDeath
  , ephemeralExhibits
  , falseLead
  , frozenInFear
  , graspingHands
  , huntedDown
  , huntingShadow
  , insatiableBloodlust
  , kidnapped
  , lightOfAforgomon
  , lockedDoor
  , maskOfUmordhoth
  , maskedHorrors
  , mysteriousChanting
  , obscuringFog
  , offerOfPower
  , onTheProwl
  , onWingsOfDarkness
  , passageIntoTheVeil
  , psychopompsSong
  , pushedIntoTheBeyond
  , ripplesOnTheSurface
  , ritesHowled
  , rottingRemains
  , rottingRemainsBloodOnTheAltar
  , ruinAndDestruction
  , shadowSpawned
  , slitheringBehindYou
  , somethingInTheDrinks
  , sordidAndSilent
  , spacesBetween
  , spectralMist
  , stalkedInTheDark
  , strangeSigns
  , terrorFromBeyond
  , theCreaturesTracks
  , theYellowSign
  , theZealotsSeal
  , toweringBeasts
  , twistOfFate
  , umordhothsHunger
  , umordhothsWrath
  , unhallowedCountry
  , vastExpanse
  , vaultOfEarthlyDemise
  , visionsOfFuturesPast
  , vortexOfTime
  , wormhole
  ]

placeholderTreachery :: CardDef
placeholderTreachery =
  treachery "treachery" "Placeholder Treachery Card" Test 2

coverUp :: CardDef
coverUp = (weakness "01007" "Cover Up") { cdCardTraits = setFromList [Task] }

hospitalDebts :: CardDef
hospitalDebts =
  (weakness "01011" "Hospital Debts") { cdCardTraits = setFromList [Task] }

abandonedAndAlone :: CardDef
abandonedAndAlone = (weakness "01015" "Abandoned and Alone")
  { cdCardTraits = setFromList [Madness]
  }

amnesia :: CardDef
amnesia = (weakness "01096" "Amnesia") { cdCardTraits = setFromList [Madness] }

paranoia :: CardDef
paranoia =
  (weakness "01097" "Paranoia") { cdCardTraits = setFromList [Madness] }

haunted :: CardDef
haunted = (weakness "01098" "Haunted") { cdCardTraits = setFromList [Curse] }

psychosis :: CardDef
psychosis =
  (weakness "01099" "Psychosis") { cdCardTraits = setFromList [Madness] }

hypochondria :: CardDef
hypochondria =
  (weakness "01100" "Hypochondria") { cdCardTraits = setFromList [Madness] }

huntingShadow :: CardDef
huntingShadow = (treachery "01135" "Hunting Shadow" TheMidnightMasks 3)
  { cdCardTraits = setFromList [Curse]
  , cdKeywords = setFromList [Keyword.Peril]
  }

falseLead :: CardDef
falseLead = treachery "01136" "False Lead" TheMidnightMasks 2

umordhothsWrath :: CardDef
umordhothsWrath = (treachery "01158" "Umôrdhoth's Wrath" TheDevourerBelow 2)
  { cdCardTraits = setFromList [Curse]
  }

graspingHands :: CardDef
graspingHands = (treachery "01162" "Grasping Hands" Ghouls 3)
  { cdCardTraits = setFromList [Hazard]
  }

rottingRemains :: CardDef
rottingRemains = (treachery "01163" "Rotting Remains" StrikingFear 3)
  { cdCardTraits = setFromList [Terror]
  }

frozenInFear :: CardDef
frozenInFear = (treachery "01164" "Frozen in Fear" StrikingFear 2)
  { cdCardTraits = setFromList [Terror]
  }

dissonantVoices :: CardDef
dissonantVoices = (treachery "01165" "Dissonant Voices" StrikingFear 2)
  { cdCardTraits = setFromList [Terror]
  }

ancientEvils :: CardDef
ancientEvils = (treachery "01166" "Ancient Evils" AncientEvils 3)
  { cdCardTraits = setFromList [Omen]
  }

cryptChill :: CardDef
cryptChill = (treachery "01167" "Crypt Chill" ChillingCold 2)
  { cdCardTraits = setFromList [Hazard]
  }

obscuringFog :: CardDef
obscuringFog = (treachery "01168" "Obscuring Fog" ChillingCold 2)
  { cdCardTraits = setFromList [Hazard]
  }

mysteriousChanting :: CardDef
mysteriousChanting = (treachery "01171" "Mysterious Chanting" DarkCult 2)
  { cdCardTraits = setFromList [Hex]
  }

onWingsOfDarkness :: CardDef
onWingsOfDarkness = treachery "01173" "On Wings of Darkness" Nightgaunts 2

lockedDoor :: CardDef
lockedDoor = (treachery "01174" "Locked Door" LockedDoors 2)
  { cdCardTraits = setFromList [Obstacle]
  }

theYellowSign :: CardDef
theYellowSign = (treachery "01176" "The Yellow Sign" AgentsOfHastur 2)
  { cdCardTraits = setFromList [Omen]
  }

offerOfPower :: CardDef
offerOfPower = (treachery "01178" "Offer of Power" AgentsOfYogSothoth 2)
  { cdCardTraits = setFromList [Pact]
  , cdKeywords = setFromList [Keyword.Peril]
  }

dreamsOfRlyeh :: CardDef
dreamsOfRlyeh = (treachery "01182" "Dreams of R'lyeh" AgentsOfCthulhu 2)
  { cdCardTraits = setFromList [Omen]
  }

smiteTheWicked :: CardDef
smiteTheWicked =
  (weakness "02007" "Smite the Wicked") { cdCardTraits = setFromList [Task] }

rexsCurse :: CardDef
rexsCurse =
  (weakness "02009" "Rex's Curse") { cdCardTraits = setFromList [Curse] }

searchingForIzzie :: CardDef
searchingForIzzie =
  (weakness "02011" "Searching for Izzie") { cdCardTraits = setFromList [Task] }

finalRhapsody :: CardDef
finalRhapsody =
  (weakness "02013" "Final Rhapsody") { cdCardTraits = setFromList [Endtimes] }

wrackedByNightmares :: CardDef
wrackedByNightmares = (weakness "02015" "Wracked by Nightmares")
  { cdCardTraits = setFromList [Madness]
  }

indebted :: CardDef
indebted = (weakness "02037" "Indebted")
  { cdCardTraits = singleton Flaw
  , cdPermanent = True
  }

internalInjury :: CardDef
internalInjury =
  (weakness "02038" "Internal Injury") { cdCardTraits = singleton Injury }

chronophobia :: CardDef
chronophobia =
  (weakness "02039" "Chronophobia") { cdCardTraits = singleton Madness }

somethingInTheDrinks :: CardDef
somethingInTheDrinks =
  (treachery "02081" "Something in the Drinks" TheHouseAlwaysWins 2)
    { cdCardTraits = setFromList [Poison, Illicit]
    , cdKeywords = setFromList [Keyword.Surge]
    }

arousingSuspicions :: CardDef
arousingSuspicions =
  treachery "02082" "Arousing Suspicions" TheHouseAlwaysWins 2

visionsOfFuturesPast :: CardDef
visionsOfFuturesPast = (treachery "02083" "Visions of Futures Past" Sorcery 3)
  { cdCardTraits = setFromList [Hex]
  }

beyondTheVeil :: CardDef
beyondTheVeil = (treachery "02084" "Beyond the Veil" Sorcery 3)
  { cdCardTraits = setFromList [Hex]
  , cdKeywords = setFromList [Keyword.Surge]
  }

lightOfAforgomon :: CardDef
lightOfAforgomon = (treachery "02085" "Light of Aforgomon" BishopsThralls 2)
  { cdCardTraits = setFromList [Pact, Power]
  , cdKeywords = setFromList [Keyword.Peril]
  }

unhallowedCountry :: CardDef
unhallowedCountry =
  (treachery "02088" "Unhallowed Country" EncounterSet.Dunwich 2)
    { cdCardTraits = setFromList [Terror]
    }

sordidAndSilent :: CardDef
sordidAndSilent = (treachery "02089" "Sordid and Silent" EncounterSet.Dunwich 2
                  )
  { cdCardTraits = setFromList [Terror]
  }

eagerForDeath :: CardDef
eagerForDeath = (treachery "02091" "Eager for Death" Whippoorwills 2)
  { cdCardTraits = setFromList [Omen]
  }

cursedLuck :: CardDef
cursedLuck = (treachery "02092" "Cursed Luck" BadLuck 3)
  { cdCardTraits = setFromList [Omen]
  }

twistOfFate :: CardDef
twistOfFate = (treachery "02093" "Twist of Fate" BadLuck 3)
  { cdCardTraits = setFromList [Omen]
  }

alteredBeast :: CardDef
alteredBeast = (treachery "02096" "Altered Beast" BeastThralls 2)
  { cdCardTraits = setFromList [Power]
  }

huntedDown :: CardDef
huntedDown = (treachery "02099" "Hunted Down" NaomisCrew 2)
  { cdCardTraits = setFromList [Tactic]
  }

pushedIntoTheBeyond :: CardDef
pushedIntoTheBeyond = (treachery "02100" "Pushed into the Beyond" TheBeyond 2)
  { cdCardTraits = setFromList [Hex]
  }

terrorFromBeyond :: CardDef
terrorFromBeyond = (treachery "02101" "Terror from Beyond" TheBeyond 2)
  { cdCardTraits = setFromList [Hex, Terror]
  , cdKeywords = setFromList [Keyword.Peril]
  }

arcaneBarrier :: CardDef
arcaneBarrier = (treachery "02102" "Arcane Barrier" TheBeyond 2)
  { cdCardTraits = setFromList [Hex, Obstacle]
  }

shadowSpawned :: CardDef
shadowSpawned = (treachery "02142" "Shadow-spawned" TheMiskatonicMuseum 1)
  { cdCardTraits = singleton Power
  }

stalkedInTheDark :: CardDef
stalkedInTheDark =
  (treachery "02143" "Stalked in the Dark" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Tactic
    }

passageIntoTheVeil :: CardDef
passageIntoTheVeil =
  (treachery "02144" "Passage into the Veil" TheMiskatonicMuseum 3)
    { cdCardTraits = singleton Power
    }

ephemeralExhibits :: CardDef
ephemeralExhibits =
  (treachery "02145" "Ephemeral Exhibits" TheMiskatonicMuseum 2)
    { cdCardTraits = singleton Terror
    }

slitheringBehindYou :: CardDef
slitheringBehindYou =
  treachery "02146" "Slithering Behind You" TheMiskatonicMuseum 2

acrossSpaceAndTime :: CardDef
acrossSpaceAndTime = (weakness "02178" "Across Space and Time")
  { cdCardTraits = setFromList [Madness]
  , cdEncounterSet = Just TheEssexCountyExpress
  , cdEncounterSetQuantity = Just 4
  }

clawsOfSteam :: CardDef
clawsOfSteam = (treachery "02180" "Claws of Steam" TheEssexCountyExpress 3)
  { cdCardTraits = singleton Power
  }

brokenRails :: CardDef
brokenRails = (treachery "02181" "Broken Rails" TheEssexCountyExpress 3)
  { cdCardTraits = singleton Hazard
  }

kidnapped :: CardDef
kidnapped = treachery "02220" "Kidnapped!" BloodOnTheAltar 3

psychopompsSong :: CardDef
psychopompsSong = (treachery "02221" "Psychopomp's Song" BloodOnTheAltar 2)
  { cdCardTraits = singleton Omen
  , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
  }

strangeSigns :: CardDef
strangeSigns = (treachery "02222" "Strange Signs" BloodOnTheAltar 2)
  { cdCardTraits = singleton Omen
  }

rottingRemainsBloodOnTheAltar :: CardDef
rottingRemainsBloodOnTheAltar =
  (treachery "02223" "Rotting Remains" BloodOnTheAltar 3)
    { cdCardTraits = singleton Terror
    }

toweringBeasts :: CardDef
toweringBeasts = (treachery "02256" "Towering Beasts" UndimensionedAndUnseen 4)
  { cdKeywords = singleton Keyword.Peril
  }

ruinAndDestruction :: CardDef
ruinAndDestruction =
  (treachery "02257" "Ruin and Destruction" UndimensionedAndUnseen 3)
    { cdCardTraits = singleton Hazard
    }

attractingAttention :: CardDef
attractingAttention =
  (treachery "02258" "Attracting Attention" UndimensionedAndUnseen 2)
    { cdKeywords = singleton Keyword.Surge
    }

theCreaturesTracks :: CardDef
theCreaturesTracks =
  (treachery "02259" "The Creatures' Tracks" UndimensionedAndUnseen 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

ritesHowled :: CardDef
ritesHowled = (treachery "02296" "Rites Howled" WhereDoomAwaits 3)
  { cdCardTraits = singleton Hex
  }

spacesBetween :: CardDef
spacesBetween = (treachery "02297" "Spaces Between" WhereDoomAwaits 3)
  { cdCardTraits = setFromList [Hex, Hazard]
  }

vortexOfTime :: CardDef
vortexOfTime = (treachery "02298" "Vortex of Time" WhereDoomAwaits 3)
  { cdCardTraits = setFromList [Hex, Hazard]
  }

collapsingReality :: CardDef
collapsingReality =
  (treachery "02331" "Collapsing Reality" LostInTimeAndSpace 3)
    { cdCardTraits = setFromList [Hazard]
    }

wormhole :: CardDef
wormhole = (treachery "02332" "Wormhole" LostInTimeAndSpace 2)
  { cdCardTraits = setFromList [Hazard]
  }

vastExpanse :: CardDef
vastExpanse = (treachery "02333" "Vast Expanse" LostInTimeAndSpace 3)
  { cdCardTraits = setFromList [Terror]
  }

theZealotsSeal :: CardDef
theZealotsSeal = (treachery "50024" "The Zealot's Seal" ReturnToTheGathering 2)
  { cdCardTraits = setFromList [Hex]
  }

maskedHorrors :: CardDef
maskedHorrors = (treachery "50031" "Masked Horrors" ReturnToTheMidnightMasks 2)
  { cdCardTraits = setFromList [Power, Scheme]
  }

vaultOfEarthlyDemise :: CardDef
vaultOfEarthlyDemise =
  (treachery "50032b" "Vault of Earthly Demise" ReturnToTheDevourerBelow 1)
    { cdCardTraits = setFromList [Eldritch, Otherworld]
    }

umordhothsHunger :: CardDef
umordhothsHunger =
  (treachery "50037" "Umôrdhoth's Hunger" ReturnToTheDevourerBelow 2)
    { cdCardTraits = setFromList [Power]
    }

chillFromBelow :: CardDef
chillFromBelow = (treachery "50040" "Chill from Below" GhoulsOfUmordhoth 3)
  { cdCardTraits = setFromList [Hazard]
  }

maskOfUmordhoth :: CardDef
maskOfUmordhoth = (treachery "50043" "Mask of Umôrdhoth" TheDevourersCult 2)
  { cdCardTraits = setFromList [Item, Mask]
  }

atychiphobia :: CardDef
atychiphobia =
  (weakness "60504" "Atychiphobia") { cdCardTraits = setFromList [Madness] }

cursedSwamp :: CardDef
cursedSwamp = (treachery "81024" "Cursed Swamp" TheBayou 3)
  { cdCardTraits = setFromList [Hazard]
  }

spectralMist :: CardDef
spectralMist = (treachery "81025" "Spectral Mist" TheBayou 3)
  { cdCardTraits = setFromList [Hazard]
  }

draggedUnder :: CardDef
draggedUnder = (treachery "81026" "Dragged Under" TheBayou 4)
  { cdCardTraits = setFromList [Hazard]
  }

ripplesOnTheSurface :: CardDef
ripplesOnTheSurface = (treachery "81027" "Ripples on the Surface" TheBayou 3)
  { cdCardTraits = setFromList [Terror]
  }

curseOfTheRougarou :: CardDef
curseOfTheRougarou = (weakness "81029" "Curse of the Rougarou")
  { cdCardTraits = setFromList [Curse]
  , cdEncounterSet = Just CurseOfTheRougarou
  , cdEncounterSetQuantity = Just 1
  }

onTheProwl :: CardDef
onTheProwl = (treachery "81034" "On the Prowl" CurseOfTheRougarou 5)
  { cdKeywords = setFromList [Keyword.Surge]
  }

beastOfTheBayou :: CardDef
beastOfTheBayou = treachery "81035" "Beast of the Bayou" CurseOfTheRougarou 2

insatiableBloodlust :: CardDef
insatiableBloodlust =
  treachery "81036" "Insatiable Bloodlust" CurseOfTheRougarou 3
