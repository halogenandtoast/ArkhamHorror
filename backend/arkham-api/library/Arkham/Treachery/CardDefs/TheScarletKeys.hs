module Arkham.Treachery.CardDefs.TheScarletKeys where

import Arkham.Treachery.CardDefs.Import

selflessToAFault :: CardDef
selflessToAFault =
  (weakness "09003" "Selfless to a Fault")
    { cdCardTraits = setFromList [Flaw]
    }

deafeningSilence :: CardDef
deafeningSilence =
  (weakness "09014" "Deafening Silence")
    { cdCardTraits = setFromList [Omen]
    }

ruinedFilm :: CardDef
ruinedFilm =
  (weakness "09017" "Ruined Film")
    { cdCardTraits = setFromList [Blunder]
    }

burdenOfLeadership :: CardDef
burdenOfLeadership =
  (weakness "09020" "Burden of Leadership")
    { cdCardTraits = setFromList [Flaw]
    }

famine :: CardDef
famine =
  (treachery "09542" "Famine" DeadHeat 2)
    { cdCardTraits = setFromList [Corruption]
    }

cornered :: CardDef
cornered =
  (treachery "09543" "Cornered!" DeadHeat 2)
    { cdCardTraits = setFromList [Hazard]
    }

catAndMouse :: CardDef
catAndMouse =
  peril
    $ surge
    $ (treachery "09559" "Cat and Mouse" SanguineShadows 2)
      { cdCardTraits = setFromList [Scheme]
      }

callingCard :: CardDef
callingCard =
  (treachery "09560" "Calling Card" SanguineShadows 2)
    { cdCardTraits = setFromList [Evidence]
    }

outsmarted :: CardDef
outsmarted =
  (treachery "09561" "Outsmarted" SanguineShadows 2)
    { cdCardTraits = setFromList [Scheme]
    }

shadowedDealingsInTheDark :: CardDef
shadowedDealingsInTheDark =
  (treachery "09587" "Shadowed" DealingsInTheDark 2)
    { cdCardTraits = singleton Scheme
    }

accosted :: CardDef
accosted =
  (treachery "09588" "Accosted" DealingsInTheDark 2)
    { cdCardTraits = singleton Scheme
    }

lightOutOfVoid :: CardDef
lightOutOfVoid =
  peril
    $ (treachery "09589" "Light Out of Void" DealingsInTheDark 2)
      { cdCardTraits = singleton Hex
      }

bodySnatched :: CardDef
bodySnatched =
  (treachery "09608" "Body Snatched" DancingMad 2)
    { cdCardTraits = setFromList [Scheme, Power]
    }

crackingIce :: CardDef
crackingIce =
  (treachery "09632" "Cracking Ice" OnThinIce 4)
    { cdCardTraits = setFromList [Hazard]
    }

snowslide :: CardDef
snowslide =
  (treachery "09633" "Snowslide" OnThinIce 2)
    { cdCardTraits = setFromList [Hazard]
    }

locusPulse :: CardDef
locusPulse =
  (treachery "09658" "Locus Pulse" DogsOfWar 2)
    { cdCardTraits = setFromList [Hex]
    }

excruciate :: CardDef
excruciate =
  (treachery "09677" "Excruciate" ShadesOfSuffering 2)
    { cdCardTraits = setFromList [Hex]
    }

spiritHarvest :: CardDef
spiritHarvest =
  peril
    $ (treachery "09678" "Spirit Harvest" ShadesOfSuffering 2)
      { cdCardTraits = setFromList [Hex]
      }

conspiracyInRed :: CardDef
conspiracyInRed =
  (treachery "09717" "Conspiracy in Red" CrimsonConspiracy 2)
    { cdCardTraits = setFromList [Scheme]
    }

pinchInReality :: CardDef
pinchInReality =
  (treachery "09718" "Pinch in Reality" StrangeHappenings 2)
    { cdCardTraits = setFromList [Power]
    }

heavyRain :: CardDef
heavyRain =
  (treachery "09719" "Heavy Rain" StrangeHappenings 2)
    { cdCardTraits = setFromList [Hazard]
    }

inPlainSight :: CardDef
inPlainSight =
  (treachery "09721" "In Plain Sight" MysteriesAbound 2)
    { cdCardTraits = setFromList [Scheme]
    }

knivesInTheDark :: CardDef
knivesInTheDark =
  (treachery "09722" "Knives in the Dark" ShadowOfADoubt 2)
    { cdCardTraits = setFromList [Scheme]
    }

undercover :: CardDef
undercover =
  (treachery "09723" "Undercover" ShadowOfADoubt 2)
    { cdCardTraits = setFromList [Scheme]
    }

seeingShadows :: CardDef
seeingShadows =
  (treachery "09724" "Seeing Shadows" DarkVeiling 2)
    { cdCardTraits = setFromList [Terror]
    }

figuresInTheDark :: CardDef
figuresInTheDark =
  peril
    (treachery "09725" "Figures in the Dark" DarkVeiling 2)
      { cdCardTraits = setFromList [Scheme]
      }

swiftRetreat :: CardDef
swiftRetreat =
  (treachery "09728" "Swift Retreat" CleanupCrew 2)
    { cdCardTraits = setFromList [Scheme]
    }

boundInRed :: CardDef
boundInRed =
  (treachery "09729" "Bound in Red" ScarletSorcery 2)
    { cdCardTraits = setFromList [Hex]
    }

keyCharge :: CardDef
keyCharge =
  surge
    $ (treachery "09730" "Key Charge" ScarletSorcery 2)
      { cdCardTraits = setFromList [Hex]
      }

substanceDissimulation :: CardDef
substanceDissimulation =
  (treachery "09733" "Substance Dissimulation" Outsiders 2)
    { cdCardTraits = setFromList [Power]
    }

memoryVariant :: CardDef
memoryVariant =
  (treachery "09735" "Memory Variant" SecretWar 2)
    { cdCardTraits = setFromList [Power]
    }

secretsLost :: CardDef
secretsLost =
  (treachery "09736" "Secrets Lost" SecretWar 3)
    { cdCardTraits = setFromList [Power]
    }

matterInversion :: CardDef
matterInversion =
  (treachery "09738" "Matter Inversion" AgentsOfTheOutside 2)
    { cdCardTraits = setFromList [Power]
    }

tenebrousEclipse :: CardDef
tenebrousEclipse =
  (treachery "09740" "Tenebrous Eclipse" AgentsOfYuggoth 3)
    { cdCardTraits = setFromList [Omen]
    }

splinteredSpace :: CardDef
splinteredSpace =
  (treachery "09741" "Splintered Space" SpatialAnomaly 3)
    { cdCardTraits = setFromList [Hex]
    }

beyondThePale :: CardDef
beyondThePale =
  (treachery "09742" "Beyond the Pale" SpatialAnomaly 3)
    { cdCardTraits = setFromList [Hex]
    }

warpedReality :: CardDef
warpedReality =
  (treachery "09743" "Warped Reality" SpatialAnomaly 2)
    { cdCardTraits = setFromList [Hex]
    }

touchOfTheBeyond :: CardDef
touchOfTheBeyond =
  (treachery "09744" "Touch of the Beyond" SpreadingCorruption 2)
    { cdCardTraits = setFromList [Curse]
    }

compulsion :: CardDef
compulsion =
  (treachery "09745" "Compulsion" SpreadingCorruption 2)
    { cdCardTraits = setFromList [Curse, Terror]
    }

distortedReasoning :: CardDef
distortedReasoning =
  (treachery "09746" "Distorted Reasoning" SpreadingCorruption 2)
    { cdCardTraits = setFromList [Curse, Terror]
    }

paradimensionalTerror :: CardDef
paradimensionalTerror =
  peril
    $ (treachery "09751" "Paradimensional Terror" BeyondTheBeyond 2)
      { cdCardTraits = setFromList [Terror]
      }

paradimensionalUnderstanding :: CardDef
paradimensionalUnderstanding =
  (weakness "09767" "Paradimensional Understanding")
    { cdCardTraits = setFromList [Madness, Paradox]
    , cdEncounterSet = Just Globetrotting
    , cdEncounterSetQuantity = Just 4
    }
