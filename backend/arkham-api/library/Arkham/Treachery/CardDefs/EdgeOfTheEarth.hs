{- HLINT ignore "Use camelCase" -}
module Arkham.Treachery.CardDefs.EdgeOfTheEarth where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

theHarbinger :: CardDef
theHarbinger =
  (weakness "08006" "The Harbinger")
    { cdCardTraits = setFromList [Omen, Endtimes]
    , cdOutOfPlayEffects = [OnTopOfDeckEffect]
    }

buriedSecrets :: CardDef
buriedSecrets =
  (weakness "08009" "Buried Secrets")
    { cdCardTraits = setFromList [Mystery]
    }

burdenOfDestiny :: CardDef
burdenOfDestiny =
  (weakness "08015" "Burden of Destiny")
    { cdCardTraits = setFromList [Flaw]
    }

greed :: CardDef
greed =
  (weakness "08018" "Greed")
    { cdCardTraits = setFromList [Flaw]
    }

armInjury :: CardDef
armInjury =
  (basicWeakness "08130" "Arm Injury")
    { cdCardTraits = singleton Injury
    }

legInjury :: CardDef
legInjury =
  (basicWeakness "08131" "Leg Injury")
    { cdCardTraits = singleton Injury
    }

panic :: CardDef
panic =
  (basicWeakness "08132" "Panic")
    { cdCardTraits = singleton Madness
    }

stupor :: CardDef
stupor =
  (basicWeakness "08133" "Stupor")
    { cdCardTraits = singleton Madness
    }

apeirophobia :: CardDef
apeirophobia =
  (treachery "08516" "Apeirophobia" IceAndDeath 2)
    { cdCardTraits = setFromList [Terror]
    }

zeroVisibility :: CardDef
zeroVisibility =
  (treachery "08517" "Zero Visibility" IceAndDeath 2)
    { cdCardTraits = setFromList [Hazard]
    }

phantasmagoria :: CardDef
phantasmagoria =
  (treachery "08548" "Phantasmagoria" SeepingNightmares 2)
    { cdCardTraits = setFromList [Curse]
    }

evanescentMist :: CardDef
evanescentMist =
  (treachery "08585" "Evanescent Mist" FatalMirage 3)
    { cdCardTraits = setFromList [Curse, Hazard]
    }

anamnesis :: CardDef
anamnesis =
  (treachery "08586" "Anamnesis" FatalMirage 3)
    { cdCardTraits = setFromList [Terror]
    , cdKeywords = setFromList [Keyword.Peril]
    }

snowfall :: CardDef
snowfall =
  (treachery "08610" "Snowfall" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Hazard]
    }

avalanche :: CardDef
avalanche =
  (treachery "08611" "Avalance" ToTheForbiddenPeaks 2)
    { cdCardTraits = setFromList [Hazard]
    }

hangingOnTheEdge :: CardDef
hangingOnTheEdge =
  (treachery "08612" "Hanging on the Edge" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Hazard]
    }

hypothermia :: CardDef
hypothermia =
  (treachery "08613" "Hypothermia" ToTheForbiddenPeaks 3)
    { cdCardTraits = setFromList [Hazard]
    }

dawningOfTheTruth :: CardDef
dawningOfTheTruth =
  (treachery "08644" "Dawning of the Truth" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Terror]
    }

crumblingRuins :: CardDef
crumblingRuins =
  (treachery "08645" "Crumbling Ruins" CityOfTheElderThings 3)
    { cdCardTraits = setFromList [Hazard]
    }

frostbitten :: CardDef
frostbitten =
  (weakness "08646" "Frostbitten")
    { cdCardTraits = setFromList [Injury]
    , cdEncounterSet = Just CityOfTheElderThings
    , cdEncounterSetQuantity = Just 4
    }

possessed :: CardDef
possessed =
  (weakness "08647" "Possessed")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just CityOfTheElderThings
    , cdEncounterSetQuantity = Just 4
    }

primevalTerror :: CardDef
primevalTerror =
  (treachery "08657" "Primeval Terror" TheHeartOfMadness 3)
    { cdCardTraits = setFromList [Terror]
    }

rootsOfTheEarth :: CardDef
rootsOfTheEarth =
  (treachery "08658" "Roots of the Earth" TheHeartOfMadness 3)
    { cdCardTraits = setFromList [Hazard]
    }

electrostaticDischarge :: CardDef
electrostaticDischarge =
  (treachery "08670" "Electrostatic Discarge" TheGreatSeal 2)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Surge]
    }

theMadnessWithin :: CardDef
theMadnessWithin =
  (treachery "08688" "The Madness Within" AgentsOfTheUnknown 2)
    { cdCardTraits = setFromList [Curse]
    }

kindredMist :: CardDef
kindredMist =
  (treachery "08691" "Kindred Mist" CreaturesInTheIce 2)
    { cdCardTraits = setFromList [Curse]
    }

antarcticWind :: CardDef
antarcticWind =
  (treachery "08692" "Antarctic Wind" DeadlyWeather 2)
    { cdCardTraits = setFromList [Hazard]
    }

whiteout :: CardDef
whiteout =
  (treachery "08693" "Whiteout" DeadlyWeather 2)
    { cdCardTraits = setFromList [Hazard]
    }

polarVortex :: CardDef
polarVortex =
  (treachery "08694" "Polar Vortex" DeadlyWeather 2)
    { cdCardTraits = setFromList [Hazard]
    }

riseOfTheElderThings :: CardDef
riseOfTheElderThings =
  (treachery "08697" "Rise of the Elder Things" ElderThings 2)
    { cdCardTraits = setFromList [Hazard]
    }

iceShaft :: CardDef
iceShaft =
  (treachery "08698" "Ice Shaft" HazardsOfAntarctica 3)
    { cdCardTraits = setFromList [Hazard]
    }

throughTheIce :: CardDef
throughTheIce =
  (treachery "08699" "Through the Ice" HazardsOfAntarctica 2)
    { cdCardTraits = setFromList [Hazard]
    }

abandonedToMadness :: CardDef
abandonedToMadness =
  (treachery "08702" "Abandoned to Madness" LeftBehind 2)
    { cdCardTraits = setFromList [Curse]
    }

blasphemousVisions :: CardDef
blasphemousVisions =
  (treachery "08703" "Blasphemous Visions" NamelessHorrors 2)
    { cdCardTraits = setFromList [Terror]
    }

glimpseTheUnspeakable :: CardDef
glimpseTheUnspeakable =
  peril
    $ (treachery "08704" "Glimpse the Unspeakable" NamelessHorrors 2)
      { cdCardTraits = setFromList [Terror]
      }

nightmarishVapors :: CardDef
nightmarishVapors =
  peril
    $ (treachery "08705" "Nightmarish Vapors" NamelessHorrors 2)
      { cdCardTraits = setFromList [Terror]
      }

miasmaticTorment :: CardDef
miasmaticTorment =
  (treachery "08706" "Miasmatic Torment" Miasma 2)
    { cdCardTraits = setFromList [Curse]
    }

nebulousMiasma :: CardDef
nebulousMiasma =
  (treachery "08707" "Nebulous Miasma" Miasma 2)
    { cdCardTraits = setFromList [Curse, Hazard]
    }

wukWukWuk :: CardDef
wukWukWuk =
  (treachery "08709" "Wuk! Wuk! Wuk!" Penguins 2)
    { cdCardTraits = setFromList [Curse, Hazard]
    }

polarMirage :: CardDef
polarMirage =
  (treachery "08712" "Polar Mirage" SilenceAndMystery 2)
    { cdCardTraits = setFromList [Terror]
    }

darkAurora :: CardDef
darkAurora =
  (treachery "08713" "Dark Aurora" SilenceAndMystery 3)
    { cdCardTraits = setFromList [Terror]
    }

tekelili_223 :: CardDef
tekelili_223 =
  (weakness "08723" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 3
    }

tekelili_224 :: CardDef
tekelili_224 =
  (weakness "08724" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 3
    }

tekelili_225 :: CardDef
tekelili_225 =
  (weakness "08725" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 2
    }

tekelili_226 :: CardDef
tekelili_226 =
  (weakness "08726" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 2
    }

tekelili_227 :: CardDef
tekelili_227 =
  (weakness "08727" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 2
    }

tekelili_228 :: CardDef
tekelili_228 =
  (weakness "08728" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 2
    }

tekelili_229 :: CardDef
tekelili_229 =
  (weakness "08729" "Tekeli-li")
    { cdCardTraits = setFromList [Madness]
    , cdEncounterSet = Just Tekelili
    , cdEncounterSetQuantity = Just 2
    }
