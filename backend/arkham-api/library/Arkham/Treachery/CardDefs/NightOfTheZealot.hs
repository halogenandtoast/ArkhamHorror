module Arkham.Treachery.CardDefs.NightOfTheZealot where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

coverUp :: CardDef
coverUp =
  (weakness "01007" "Cover Up")
    { cdCardTraits = setFromList [Task]
    , cdAlternateCardCodes = ["01507"]
    }

hospitalDebts :: CardDef
hospitalDebts =
  (weakness "01011" "Hospital Debts")
    { cdCardTraits = setFromList [Task]
    , cdAlternateCardCodes = ["01511"]
    }

abandonedAndAlone :: CardDef
abandonedAndAlone =
  (weakness "01015" "Abandoned and Alone")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01515"]
    }

amnesia :: CardDef
amnesia =
  (basicWeakness "01096" "Amnesia")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01596", "12097"]
    }

paranoia :: CardDef
paranoia =
  (basicWeakness "01097" "Paranoia")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01597", "12101"]
    }

haunted :: CardDef
haunted =
  (basicWeakness "01098" "Haunted")
    { cdCardTraits = setFromList [Curse]
    , cdAlternateCardCodes = ["01598"]
    }

psychosis :: CardDef
psychosis =
  (basicWeakness "01099" "Psychosis")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01599"]
    }

hypochondria :: CardDef
hypochondria =
  (basicWeakness "01100" "Hypochondria")
    { cdCardTraits = setFromList [Madness]
    , cdAlternateCardCodes = ["01600"]
    }

huntingShadow :: CardDef
huntingShadow =
  (treachery "01135" "Hunting Shadow" TheMidnightMasks 3)
    { cdCardTraits = setFromList [Curse]
    , cdKeywords = setFromList [Keyword.Peril]
    }

falseLead :: CardDef
falseLead = treachery "01136" "False Lead" TheMidnightMasks 2

umordhothsWrath :: CardDef
umordhothsWrath =
  (treachery "01158" "Umôrdhoth's Wrath" TheDevourerBelow 2)
    { cdCardTraits = setFromList [Curse]
    }

graspingHands :: CardDef
graspingHands =
  (treachery "01162" "Grasping Hands" Ghouls 3)
    { cdCardTraits = setFromList [Hazard]
    }

rottingRemains :: CardDef
rottingRemains =
  (treachery "01163" "Rotting Remains" StrikingFear 3)
    { cdCardTraits = setFromList [Terror]
    }

frozenInFear :: CardDef
frozenInFear =
  (treachery "01164" "Frozen in Fear" StrikingFear 2)
    { cdCardTraits = setFromList [Terror]
    }

dissonantVoices :: CardDef
dissonantVoices =
  (treachery "01165" "Dissonant Voices" StrikingFear 2)
    { cdCardTraits = setFromList [Terror]
    }

ancientEvils :: CardDef
ancientEvils =
  (treachery "01166" "Ancient Evils" AncientEvils 3)
    { cdCardTraits = setFromList [Omen]
    }

cryptChill :: CardDef
cryptChill =
  (treachery "01167" "Crypt Chill" ChillingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }

obscuringFog :: CardDef
obscuringFog =
  (treachery "01168" "Obscuring Fog" ChillingCold 2)
    { cdCardTraits = setFromList [Hazard]
    }

mysteriousChanting :: CardDef
mysteriousChanting =
  (treachery "01171" "Mysterious Chanting" DarkCult 2)
    { cdCardTraits = setFromList [Hex]
    }

onWingsOfDarkness :: CardDef
onWingsOfDarkness = treachery "01173" "On Wings of Darkness" Nightgaunts 2

lockedDoor :: CardDef
lockedDoor =
  (treachery "01174" "Locked Door" LockedDoors 2)
    { cdCardTraits = setFromList [Obstacle]
    }

theYellowSign :: CardDef
theYellowSign =
  (treachery "01176" "The Yellow Sign" AgentsOfHastur 2)
    { cdCardTraits = setFromList [Omen]
    }

offerOfPower :: CardDef
offerOfPower =
  (treachery "01178" "Offer of Power" AgentsOfYogSothoth 2)
    { cdCardTraits = setFromList [Pact]
    , cdKeywords = setFromList [Keyword.Peril]
    }

dreamsOfRlyeh :: CardDef
dreamsOfRlyeh =
  (treachery "01182" "Dreams of R'lyeh" AgentsOfCthulhu 2)
    { cdCardTraits = setFromList [Omen]
    }
