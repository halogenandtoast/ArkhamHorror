module Arkham.Treachery.CardDefs.TheInnsmouthConspiracy where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

crisisOfFaith :: CardDef
crisisOfFaith =
  (weakness "07007" "Crisis of Faith")
    { cdCardTraits = singleton Madness
    }

sirenCall :: CardDef
sirenCall =
  (weakness "07016" "Siren Call")
    { cdCardTraits = singleton Curse
    }

dreadCurse :: CardDef
dreadCurse =
  (basicWeakness "07039" "Dread Curse")
    { cdCardTraits = singleton Curse
    }

dayOfReckoning :: CardDef
dayOfReckoning =
  (basicWeakness "07040" "Day of Reckoning")
    { cdCardTraits = singleton Endtimes
    }

blindsense :: CardDef
blindsense =
  (treachery "07054" "Blindsense" ThePitOfDespair 2)
    { cdCardTraits = singleton Scheme
    }

fromTheDepths :: CardDef
fromTheDepths =
  (treachery "07055" "From the Depths" ThePitOfDespair 3)
    { cdCardTraits = singleton Scheme
    }

psychicPull :: CardDef
psychicPull =
  (treachery "07087" "Psychic Pull" AgentsOfHydra 3)
    { cdCardTraits = singleton Power
    }

deepOneAssault :: CardDef
deepOneAssault =
  (treachery "07090" "Deep One Assault" CreaturesOfTheDeep 2)
    { cdCardTraits = singleton Scheme
    }

undertow :: CardDef
undertow =
  (treachery "07091" "Undertow" RisingTide 2)
    { cdCardTraits = singleton Hazard
    }

risingTides :: CardDef
risingTides =
  (treachery "07092" "Rising Tides" RisingTide 2)
    { cdCardTraits = singleton Hazard
    }

riptide :: CardDef
riptide =
  (treachery "07093" "Riptide" RisingTide 2)
    { cdCardTraits = singleton Hazard
    }

fogOverInnsmouth :: CardDef
fogOverInnsmouth =
  (treachery "07095" "Fog over Innsmouth" FogOverInnsmouth 2)
    { cdCardTraits = singleton Hazard
    }

macabreMemento :: CardDef
macabreMemento =
  (treachery "07096" "Macabre Memento" ShatteredMemories 2)
    { cdCardTraits = singleton Terror
    }

fracturedConsciousness :: CardDef
fracturedConsciousness =
  (treachery "07097" "Fractured Consciousness" ShatteredMemories 2)
    { cdCardTraits = singleton Terror
    }

memoryOfOblivion :: CardDef
memoryOfOblivion =
  (treachery "07098" "Memory of Oblivion" ShatteredMemories 2)
    { cdCardTraits = singleton Terror
    }

malfunction :: CardDef
malfunction =
  (treachery "07099" "Malfunction" Malfunction 2)
    { cdCardTraits = singleton Blunder
    }

tidalAlignment :: CardDef
tidalAlignment =
  (treachery "07100" "Tidal Alignment" Syzygy 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = singleton Keyword.Peril
    }

syzygy :: CardDef
syzygy =
  (treachery "07101" "Syzygy" Syzygy 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = singleton Keyword.Peril
    }

innsmouthLook :: CardDef
innsmouthLook =
  (treachery "07106" "Innsmouth Look" TheLocals 2)
    { cdCardTraits = setFromList [Curse, Terror]
    }

furtiveLocals :: CardDef
furtiveLocals =
  (treachery "07107" "Furtive Locals" TheLocals 2)
    { cdCardTraits = singleton Terror
    }

deepOneInvasion :: CardDef
deepOneInvasion =
  (treachery "07147" "Deep One Invasion" InTooDeep 1)
    { cdCardTraits = singleton Scheme
    }

pulledBack :: CardDef
pulledBack =
  (treachery "07148" "Pulled Back" InTooDeep 2)
    { cdCardTraits = singleton Terror
    }

inundated :: CardDef
inundated =
  (treachery "07149" "Inundated" InTooDeep 3)
    { cdCardTraits = singleton Hazard
    }

shapesInTheWater :: CardDef
shapesInTheWater =
  (treachery "07184" "Shapes in the Water" DevilReef 2)
    { cdCardTraits = singleton Terror
    }

aquaticAmbush :: CardDef
aquaticAmbush =
  (treachery "07185" "Aquatic Ambush" DevilReef 2)
    { cdCardTraits = singleton Scheme
    }

horrorsFromTheDeep :: CardDef
horrorsFromTheDeep =
  (treachery "07186" "Horrors from the Deep" DevilReef 2)
    { cdCardTraits = singleton Terror
    }

stowaway :: CardDef
stowaway =
  (treachery "07187" "Stowaway" DevilReef 2)
    { cdCardTraits = singleton Scheme
    }

draggedUnderDevilReef :: CardDef
draggedUnderDevilReef =
  (treachery "07188" "Dragged Under" DevilReef 3)
    { cdCardTraits = setFromList [Scheme, Terror]
    }

bumpyRide :: CardDef
bumpyRide =
  (treachery "07216" "Bumpy Ride" HorrorInHighGear 2)
    { cdCardTraits = singleton Hazard
    }

iCantSee :: CardDef
iCantSee =
  (treachery "07217" "\"I can't see\"" HorrorInHighGear 2)
    { cdCardTraits = singleton Hazard
    }

eyesInTheTrees :: CardDef
eyesInTheTrees =
  (treachery "07218" "Eyes in the Trees" HorrorInHighGear 2)
    { cdCardTraits = singleton Hazard
    }

theyreCatchingUp :: CardDef
theyreCatchingUp =
  (treachery "07219" "\"They're catching up!\"" HorrorInHighGear 2)
    { cdCardTraits = singleton Scheme
    }

hideousLullaby :: CardDef
hideousLullaby =
  (treachery "07256" "Hideous Lullaby" ALightInTheFog 3)
    { cdCardTraits = singleton Terror
    }

kissOfBrine :: CardDef
kissOfBrine =
  (treachery "07257" "Kiss of Brine" ALightInTheFog 2)
    { cdCardTraits = setFromList [Curse, Hazard]
    }

totality :: CardDef
totality =
  (treachery "07258" "Totality" ALightInTheFog 2)
    { cdCardTraits = setFromList [Omen, Terror]
    }

worthHisSalt :: CardDef
worthHisSalt =
  (treachery "07259" "Worth His Salt" ALightInTheFog 2)
    { cdCardTraits = singleton Scheme
    }

takenCaptive :: CardDef
takenCaptive =
  (treachery "07260" "Taken Captive" ALightInTheFog 2)
    { cdCardTraits = singleton Scheme
    }

fulfillTheOaths :: CardDef
fulfillTheOaths =
  (treachery "07295" "Fulfill the Oaths" TheLairOfDagon 3)
    { cdCardTraits = setFromList [Hazard]
    }

secretGathering :: CardDef
secretGathering =
  (treachery "07296" "Secret Gathering" TheLairOfDagon 2)
    { cdCardTraits = setFromList [Hex]
    }

esotericRitual :: CardDef
esotericRitual =
  (treachery "07297" "Esoteric Ritual" TheLairOfDagon 3)
    { cdCardTraits = setFromList [Hex]
    }

heraldsOfTheDeep :: CardDef
heraldsOfTheDeep =
  (treachery "07298" "Heralds of the Deep" TheLairOfDagon 3)
    { cdCardTraits = setFromList [Curse]
    }

stoneBarrier :: CardDef
stoneBarrier =
  (treachery "07299" "Stone Barrier" TheLairOfDagon 2)
    { cdCardTraits = setFromList [Obstacle]
    }

treacherousDepths :: CardDef
treacherousDepths =
  (treachery "07335" "Treacherous Depths" IntoTheMaelstrom 3)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Keyword.Peril]
    }

conspiracyOfDeepOnes :: CardDef
conspiracyOfDeepOnes =
  (treachery "07336" "Conspiracy of Deep Ones" IntoTheMaelstrom 2)
    { cdCardTraits = setFromList [Scheme]
    , cdKeywords = setFromList [Keyword.Peril]
    }

thalassophobia :: CardDef
thalassophobia =
  (treachery "07337" "Thalassophobia" IntoTheMaelstrom 2)
    { cdCardTraits = setFromList [Terror]
    }
