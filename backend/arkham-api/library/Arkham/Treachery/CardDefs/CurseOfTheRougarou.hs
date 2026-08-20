module Arkham.Treachery.CardDefs.CurseOfTheRougarou where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

beastOfTheBayou :: CardDef
beastOfTheBayou = treachery "81035" "Beast of the Bayou" CurseOfTheRougarou 2

curseOfTheRougarou :: CardDef
curseOfTheRougarou =
  (weakness "81029" "Curse of the Rougarou")
    { cdCardTraits = setFromList [Curse]
    , cdEncounterSet = Just CurseOfTheRougarou
    , cdEncounterSetQuantity = Just 1
    }

cursedSwamp :: CardDef
cursedSwamp =
  (treachery "81024" "Cursed Swamp" TheBayou 3)
    { cdCardTraits = setFromList [Hazard]
    }

draggedUnder :: CardDef
draggedUnder =
  (treachery "81026" "Dragged Under" TheBayou 4)
    { cdCardTraits = setFromList [Hazard]
    }

insatiableBloodlust :: CardDef
insatiableBloodlust =
  treachery "81036" "Insatiable Bloodlust" CurseOfTheRougarou 3

onTheProwl :: CardDef
onTheProwl =
  (treachery "81034" "On the Prowl" CurseOfTheRougarou 5)
    { cdKeywords = setFromList [Keyword.Surge]
    }

ripplesOnTheSurface :: CardDef
ripplesOnTheSurface =
  (treachery "81027" "Ripples on the Surface" TheBayou 3)
    { cdCardTraits = setFromList [Terror]
    }

spectralMist :: CardDef
spectralMist =
  (treachery "81025" "Spectral Mist" TheBayou 3)
    { cdCardTraits = setFromList [Hazard]
    }
