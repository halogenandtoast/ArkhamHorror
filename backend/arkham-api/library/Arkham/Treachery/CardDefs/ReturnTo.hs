module Arkham.Treachery.CardDefs.ReturnTo where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theZealotsSeal :: CardDef
theZealotsSeal =
  (treachery "50024" "The Zealot's Seal" ReturnToTheGathering 2)
    { cdCardTraits = setFromList [Hex]
    }

maskedHorrors :: CardDef
maskedHorrors =
  (treachery "50031" "Masked Horrors" ReturnToTheMidnightMasks 2)
    { cdCardTraits = setFromList [Power, Scheme]
    }

vaultOfEarthlyDemise :: CardDef
vaultOfEarthlyDemise =
  (treachery "50032b" "Vault of Earthly Demise" ReturnToTheDevourerBelow 1)
    { cdCardTraits = setFromList [Eldritch, Otherworld]
    , cdOtherSide = Just "50032"
    , cdDoubleSided = True
    }

umordhothsHunger :: CardDef
umordhothsHunger =
  (treachery "50037" "Umôrdhoth's Hunger" ReturnToTheDevourerBelow 2)
    { cdCardTraits = setFromList [Power]
    }

chillFromBelow :: CardDef
chillFromBelow =
  (treachery "50040" "Chill from Below" GhoulsOfUmordhoth 3)
    { cdCardTraits = setFromList [Hazard]
    }

maskOfUmordhoth :: CardDef
maskOfUmordhoth =
  (treachery "50043" "Mask of Umôrdhoth" TheDevourersCult 2)
    { cdCardTraits = setFromList [Item, Mask]
    }

throughTheGates :: CardDef
throughTheGates =
  (basicWeakness "51011" "Through the Gates") {cdCardTraits = setFromList [Pact, Mystery]}

caughtCheating :: CardDef
caughtCheating =
  surge
    $ (treachery "51018" "Caught Cheating" ReturnToTheHouseAlwaysWins 2)
      { cdCardTraits = setFromList [Illicit]
      }

raiseTheStakes :: CardDef
raiseTheStakes =
  (treachery "51019" "Raise the Stakes" ReturnToTheHouseAlwaysWins 2)
    { cdCardTraits = setFromList [Illicit]
    }

darkBidding :: CardDef
darkBidding =
  (treachery "51023" "Dark Bidding" ReturnToTheMiskatonicMuseum 2)
    { cdCardTraits = setFromList [Power]
    }

nightBeyondVoid :: CardDef
nightBeyondVoid =
  (treachery "51024" "Night Beyond Void" ReturnToTheMiskatonicMuseum 2)
    { cdCardTraits = setFromList [Power]
    , cdVictoryPoints = Just 0
    }

imperceptableCreature :: CardDef
imperceptableCreature =
  (treachery "51046" "Imperceptable Creature" ReturnToUndimensionedAndUnseen 2)
    { cdCardTraits = setFromList [Power]
    , cdKeywords = setFromList [Keyword.Surge]
    }

hauntingRecollections :: CardDef
hauntingRecollections =
  (treachery "51061" "Haunting Recollections" BeyondTheThreshold 2)
    { cdCardTraits = setFromList [Hex]
    }

aBalefulWelcome :: CardDef
aBalefulWelcome =
  (treachery "51062" "A Baleful Welcome" BeyondTheThreshold 2)
    { cdCardTraits = setFromList [Hex]
    , cdKeywords = setFromList [Keyword.Peril]
    }

infiniteDoorway :: CardDef
infiniteDoorway =
  (treachery "51063" "Infinite Doorway" BeyondTheThreshold 2)
    { cdCardTraits = setFromList [Hex]
    }

resurgentEvils :: CardDef
resurgentEvils =
  peril
    $ (treachery "51064" "Resurgent Evils" ResurgentEvils 3)
      { cdCardTraits = setFromList [Omen]
      }

secretDoor :: CardDef
secretDoor =
  (treachery "51065" "Secret Door" SecretDoors 2)
    { cdCardTraits = setFromList [Obstacle]
    }

inexplicableCold :: CardDef
inexplicableCold =
  (treachery "51066" "Inexplicable Cold" CreepingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }

oppressiveMists :: CardDef
oppressiveMists =
  (treachery "51067" "Oppressive Mists" CreepingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }

violentCommands :: CardDef
violentCommands =
  (treachery "51068" "Violent Commands" ErraticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

idleHands :: CardDef
idleHands =
  (treachery "51069" "Idle Hands" ErraticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

needForKnowledge :: CardDef
needForKnowledge =
  (treachery "51070" "Need for Knowledge" ErraticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

eldritchAccord :: CardDef
eldritchAccord =
  peril
    $ (treachery "51072" "Eldritch Accord" YogSothothsEmissaries 2)
      { cdCardTraits = setFromList [Pact]
      }

shockingDisplay :: CardDef
shockingDisplay =
  (treachery "52027" "Shocking Display" ReturnToTheLastKing 1)
    { cdCardTraits = setFromList [Terror]
    , cdVictoryPoints = Just 0
    }

radicalTreatment :: CardDef
radicalTreatment =
  (treachery "52038" "Radical Treatment" ReturnToTheUnspeakableOath 1)
    { cdVictoryPoints = Just 1
    , cdRevelation = NoRevelation
    }

cloudedMemory :: CardDef
cloudedMemory =
  peril
    (treachery "52039" "Clouded Memory" ReturnToTheUnspeakableOath 1)
      { cdCardTraits = setFromList [Terror]
      }

figureInTheShadows :: CardDef
figureInTheShadows =
  (treachery "52047" "Figure in the Shadows" ReturnToAPhantomOfTruth 2)
    { cdCardTraits = setFromList [Scheme]
    }

hastursGaze :: CardDef
hastursGaze =
  peril
    $ hidden
    $ (treachery "52057" "Hastur's Gaze" ReturnToBlackStarsRise 1)
      { cdCardTraits = setFromList [Power]
      }

hastursGrasp :: CardDef
hastursGrasp =
  peril
    $ hidden
    $ (treachery "52058" "Hastur's Grasp" ReturnToBlackStarsRise 1)
      { cdCardTraits = setFromList [Power]
      }

delusoryEvils :: CardDef
delusoryEvils =
  hidden
    $ peril
    $ (treachery "52065" "Delusory Evils" DelusoryEvils 3)
      { cdCardTraits = setFromList [Curse]
      }

bleedingWalls :: CardDef
bleedingWalls =
  (treachery "52066" "Bleeding Walls" DecayingReality 2)
    { cdCardTraits = setFromList [Terror]
    }

fragileThoughts :: CardDef
fragileThoughts =
  (treachery "52067" "Fragile Thoughts" DecayingReality 2)
    { cdCardTraits = setFromList [Terror]
    }

theSignOfHastur :: CardDef
theSignOfHastur =
  peril
    (treachery "52070" "The Sign of Hastur" HastursEnvoys 2)
      { cdCardTraits = setFromList [Pact, Power]
      }

visionsInYourMindHorrors :: CardDef
visionsInYourMindHorrors =
  (treachery "52071" ("Visions in Your Mind" <:> "Horrors") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

visionsInYourMindFailure :: CardDef
visionsInYourMindFailure =
  (treachery "52072" ("Visions in Your Mind" <:> "Failure") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

visionsInYourMindDeath :: CardDef
visionsInYourMindDeath =
  (treachery "52073" ("Visions in Your Mind" <:> "Death") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

visionsInYourMindHatred :: CardDef
visionsInYourMindHatred =
  (treachery "52074" ("Visions in Your Mind" <:> "Hatred") MaddeningDelusions 1)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

maddeningDelusions :: CardDef
maddeningDelusions =
  surge
    (treachery "52075" "Maddening Delusions" MaddeningDelusions 2)
      { cdCardTraits = setFromList [Terror]
      }

voiceOfTrunembra :: CardDef
voiceOfTrunembra =
  (treachery "52076" "Voice of Tru'nembra" NeuroticFear 3)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril]
    }

melancholy :: CardDef
melancholy =
  (treachery "52077" "Melancholy" NeuroticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

painfulReflection :: CardDef
painfulReflection =
  (treachery "52078" "Painful Reflection" NeuroticFear 2)
    { cdCardTraits = setFromList [Terror]
    }

unspeakableOathBloodthirst :: CardDef
unspeakableOathBloodthirst =
  (basicWeakness "52011" ("Unspeakable Oath" <:> "Bloodthirst"))
    { cdCardTraits = setFromList [Madness, Pact]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdDeckRestrictions = [CampaignModeOnly]
    }

unspeakableOathCuriosity :: CardDef
unspeakableOathCuriosity =
  (basicWeakness "52012" ("Unspeakable Oath" <:> "Curiosity"))
    { cdCardTraits = setFromList [Madness, Pact]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdDeckRestrictions = [CampaignModeOnly]
    }

unspeakableOathCowardice :: CardDef
unspeakableOathCowardice =
  (basicWeakness "52013" ("Unspeakable Oath" <:> "Cowardice"))
    { cdCardTraits = setFromList [Madness, Pact]
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    , cdDeckRestrictions = [CampaignModeOnly]
    }

offerYouCannotRefuse :: CardDef
offerYouCannotRefuse =
  (basicWeakness "53013" "Offer You Cannot Refuse")
    { cdCardTraits = singleton Pact
    , cdDeckRestrictions = [CampaignModeOnly]
    , cdGrantedXp = Just 2
    }

finePrint :: CardDef
finePrint = (weakness "53014" "Fine Print") {cdCardTraits = singleton Pact}

sellYourSoul :: CardDef
sellYourSoul = (weakness "53015" "Sell Your Soul") {cdCardTraits = singleton Pact}

perilsOfYoth :: CardDef
perilsOfYoth =
  peril
    (treachery "53060" "Perils of Yoth" ReturnToTheDepthsOfYoth 1)
      { cdCardTraits = setFromList [Hazard]
      }

unknowablePast :: CardDef
unknowablePast =
  peril
    (treachery "53065" "Unknowable Past" ReturnToShatteredAeons 2)
      { cdCardTraits = setFromList [Hex]
      }

fromAnotherTime :: CardDef
fromAnotherTime =
  (treachery "53073" "From Another Time" CultOfPnakotus 2)
    { cdCardTraits = setFromList [Scheme]
    }

resentfulWilds :: CardDef
resentfulWilds =
  (treachery "53074" "Resentful Wilds" DoomedExpedition 2)
    { cdCardTraits = setFromList [Hazard]
    , cdVengeancePoints = Just 1
    }

bestLaidPlans :: CardDef
bestLaidPlans =
  peril
    (treachery "53075" "Best-Laid Plans" DoomedExpedition 3)
      { cdCardTraits = setFromList [Blunder]
      }

mergingTimelines :: CardDef
mergingTimelines =
  (treachery "53076" "Merging Timelines" TemporalHunters 3)
    { cdCardTraits = setFromList [Hex]
    }

wrathOfYig :: CardDef
wrathOfYig =
  (treachery "53080" "Wrath of Yig" VenomousHate 1)
    { cdCardTraits = setFromList [Power]
    }

damned :: CardDef
damned =
  (basicWeakness "54014" "Damned")
    { cdCardTraits = setFromList [Curse, Omen]
    , cdPermanent = True
    }

witchweed :: CardDef
witchweed =
  peril
    (treachery "54040" "Witchweed" ReturnToTheWagesOfSin 2)
      { cdCardTraits = setFromList [Curse]
      }

brazierEnchantment :: CardDef
brazierEnchantment =
  (treachery "54048" "Brazier Enchantment" ReturnToUnionAndDisillusion 2)
    { cdCardTraits = setFromList [Curse, Hex]
    }

trespasser :: CardDef
trespasser =
  (treachery "54062" "Trespasser!" Hexcraft 3)
    { cdCardTraits = setFromList [Curse]
    }

despoiled :: CardDef
despoiled =
  (treachery "54063" "Despoiled" Hexcraft 2)
    { cdCardTraits = setFromList [Hex]
    }

maligned :: CardDef
maligned =
  (treachery "54064" "Maligned" Hexcraft 2)
    { cdCardTraits = setFromList [Hex]
    }

impendingEvils :: CardDef
impendingEvils =
  peril
    (treachery "54065" "Impending Evils" ImpendingEvils 3)
      { cdCardTraits = setFromList [Omen]
      }

unavoidableDemise :: CardDef
unavoidableDemise =
  (treachery "54066" "Unavoidable Demise" UnspeakableFate 3)
    { cdCardTraits = setFromList [Hazard, Spectral]
    }

fateOfAllFoolsUnspeakableFate :: CardDef
fateOfAllFoolsUnspeakableFate =
  (treachery "54067" "Fate of All Fools" UnspeakableFate 3)
    { cdCardTraits = setFromList [Omen, Spectral]
    }

unstableEnergies :: CardDef
unstableEnergies =
  (treachery "54068" "Unstable Energies" UnstableRealm 2)
    { cdCardTraits = setFromList [Hazard, Spectral]
    }

fromTheOtherSide :: CardDef
fromTheOtherSide =
  (treachery "54069" "From the Other Side" UnstableRealm 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }

viceAndVillainy :: CardDef
viceAndVillainy =
  (treachery "54070" "Vice and Villainy" CityOfTheDamned 2)
    { cdCardTraits = setFromList [Curse]
    }

unhallowedLand :: CardDef
unhallowedLand =
  (treachery "54071" "Unhallowed Land" CityOfTheDamned 3)
    { cdCardTraits = setFromList [Curse]
    }

supernaturalTempest :: CardDef
supernaturalTempest =
  (treachery "54072" "Supernatural Tempest" ChillingMists 2)
    { cdCardTraits = setFromList [Hazard]
    }

mistsFromBeyond :: CardDef
mistsFromBeyond =
  (treachery "54073" "Mists from Beyond" ChillingMists 2)
    { cdCardTraits = setFromList [Hazard]
    }

bloodthirstySpirits :: CardDef
bloodthirstySpirits =
  (treachery "54075" "Bloodthirsty Spiris" BloodthirstySpirits 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }
