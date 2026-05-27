module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale where

import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

hastyRepairs :: CardDef
hastyRepairs =
  (weakness "10003" "Hasty Repairs")
    { cdCardTraits = setFromList [Blunder]
    }

failedExperiment :: CardDef
failedExperiment =
  (weakness "10008" "Failed Experiment")
    { cdCardTraits = setFromList [Blunder]
    }

wheresPa :: CardDef
wheresPa =
  (weakness "10018" "\"Where's Pa?\"")
    { cdCardTraits = setFromList [Flaw]
    }

caveIn :: CardDef
caveIn =
  (treachery "10520" "Cave-In" WrittenInRock 2)
    { cdCardTraits = setFromList [Hazard]
    }

wildRide :: CardDef
wildRide =
  (treachery "10521" "Wild Ride" WrittenInRock 3)
    { cdCardTraits = setFromList [Hazard]
    }

outOfTheWalls :: CardDef
outOfTheWalls =
  (treachery "10545" "Out of the Walls" HemlockHouse 4)
    { cdCardTraits = setFromList [Hazard]
    }

pulledIn :: CardDef
pulledIn =
  (treachery "10546" "Pulled In" HemlockHouse 2)
    { cdCardTraits = setFromList [Blunder]
    }

commandingResonance :: CardDef
commandingResonance =
  (treachery "10561" "Commanding Resonanace" TheSilentHeath 2)
    { cdCardTraits = setFromList [Scheme]
    }

defendTheNest :: CardDef
defendTheNest =
  (treachery "10562" "Defend the Nest" TheSilentHeath 2)
    { cdCardTraits = setFromList [Scheme]
    }

reclaimedByNature :: CardDef
reclaimedByNature =
  (treachery "10586" "Reclaimed by Nature" TheLostSister 3)
    { cdCardTraits = setFromList [Hazard]
    }

luminousGrowth :: CardDef
luminousGrowth =
  (treachery "10587" "Luminous Growth" TheLostSister 3)
    { cdCardTraits = setFromList [Hazard, Trait.Flora]
    }

groundDisturbance :: CardDef
groundDisturbance =
  (treachery "10603" "Ground Disturbance" TheThingInTheDepths 3)
    { cdCardTraits = setFromList [Hazard]
    }

sinkingSludge :: CardDef
sinkingSludge =
  (treachery "10604" "Sinking Sludge" TheThingInTheDepths 4)
    { cdCardTraits = singleton Hazard
    }

deepShadows :: CardDef
deepShadows =
  (treachery "10622" "Deep Shadows" TheTwistedHollow 2)
    { cdCardTraits = setFromList [Hazard]
    }

lurkingFear :: CardDef
lurkingFear =
  (treachery "10623" "Lurking Fear" TheTwistedHollow 2)
    { cdCardTraits = setFromList [Terror]
    }

stolenLight :: CardDef
stolenLight =
  peril
    $ (treachery "10624" "Stolen Light" TheTwistedHollow 1)
      { cdCardTraits = setFromList [Scheme]
      }

swarm :: CardDef
swarm =
  (treachery "10676" "Swarm" TheFirstDay 3)
    { cdCardTraits = setFromList [Hazard]
    }

downpour :: CardDef
downpour =
  (treachery "10678" "Downpour" TheSecondDay 3)
    { cdCardTraits = setFromList [Hazard]
    }

otherworldlyVisions :: CardDef
otherworldlyVisions =
  (treachery "10680" "Otherworldly Visions" TheFinalDay 3)
    { cdCardTraits = setFromList [Terror, Power]
    }

chromaBlight :: CardDef
chromaBlight =
  (treachery "10722" "Chroma Blight" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Power, Trait.Blight]
    }

calcification :: CardDef
calcification =
  (treachery "10723" "Calcification" HorrorsInTheRock 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }

alienWhispers :: CardDef
alienWhispers =
  (treachery "10725" "Alien Whispers" AgentsOfTheColour 2)
    { cdCardTraits = setFromList [Power, Colour]
    }

strangeMutations :: CardDef
strangeMutations =
  (treachery "10726" "Strange Mutations" Transfiguration 2)
    { cdCardTraits = setFromList [Power]
    }

fungalRot :: CardDef
fungalRot =
  (treachery "10727" "Fungal Rot" Transfiguration 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }

enervation :: CardDef
enervation =
  (treachery "10728" "Enervation" EncounterSet.Blight 2)
    { cdCardTraits = setFromList [Hazard, Trait.Blight]
    }

desiccation :: CardDef
desiccation =
  (treachery "10729" "Desiccation" EncounterSet.Blight 2)
    { cdCardTraits = setFromList [Trait.Blight]
    }

empyreanBrilliance :: CardDef
empyreanBrilliance =
  (treachery "10730" "Empyrean Brilliance" Refractions 2)
    { cdCardTraits = setFromList [Power, Colour]
    }

captivatingGleam :: CardDef
captivatingGleam =
  surge
    $ (treachery "10731" "Captivating Gleam" Refractions 2)
      { cdCardTraits = setFromList [Power, Colour]
      }

bloom :: CardDef
bloom =
  (treachery "10735" "Bloom" TheForest 2)
    { cdCardTraits = setFromList [Power]
    }

wallOfThorns :: CardDef
wallOfThorns =
  (treachery "10736" "Wall of Thorns" TheForest 2)
    { cdCardTraits = setFromList [Hazard, Flora]
    }

callOfTheWild :: CardDef
callOfTheWild =
  (treachery "10737" "Call of the Wild" TheForest 2)
    { cdCardTraits = setFromList [Terror]
    }

psychotropicSpores :: CardDef
psychotropicSpores =
  (treachery "10740" "Psychotropic Spores" Myconids 2)
    { cdCardTraits = setFromList [Hazard, Flora]
    }

suddenMutation :: CardDef
suddenMutation =
  (treachery "10741" "Sudden Mutation" Mutations 2)
    { cdCardTraits = setFromList [Power, Colour]
    }

unnaturalGrowth :: CardDef
unnaturalGrowth =
  (treachery "10742" "Unnatural Growth" Mutations 3)
    { cdCardTraits = singleton Curse
    }

fire :: CardDef
fire =
  (treachery "10743" "Fire!" Fire 5)
    { cdCardTraits = singleton Hazard
    }

endlessNight :: CardDef
endlessNight =
  (treachery "10649" "Endless Night" TheLongestNight 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = singleton Keyword.Peril
    }

incursion :: CardDef
incursion =
  (treachery "10650" "Incursion" TheLongestNight 4)
    { cdCardTraits = singleton Scheme
    }
