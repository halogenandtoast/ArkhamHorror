module Arkham.Treachery.CardDefs.CarnevaleOfHorrors where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

abduction :: CardDef
abduction =
  (treachery "82036" "Abduction" CarnevaleOfHorrors 2)
    { cdCardTraits = singleton Scheme
    }

acridMiasma :: CardDef
acridMiasma =
  (treachery "82037" "Acrid Miasma" CarnevaleOfHorrors 2)
    { cdCardTraits = singleton Hazard
    }

chaosInTheWater :: CardDef
chaosInTheWater =
  (treachery "82034" "Chaos in the Water" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Hazard
    }

lostInVenice :: CardDef
lostInVenice =
  (treachery "82032" "Lost in Venice" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Peril
    }

massHysteria :: CardDef
massHysteria =
  (treachery "82031" "Mass Hysteria" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Hazard
    , cdKeywords = singleton Keyword.Peril
    }

mesmerize :: CardDef
mesmerize =
  (treachery "82035" "Mesmerize" CarnevaleOfHorrors 2)
    { cdCardTraits = singleton Hex
    }

watchersGaze :: CardDef
watchersGaze =
  (treachery "82033" "Watchers' Gaze" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Terror
    }
