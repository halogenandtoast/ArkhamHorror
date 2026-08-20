module Arkham.Treachery.CardDefs.TheDreamEaters.PointOfNoReturn where

import Arkham.Treachery.CardDefs.Import

dholeTunnel :: CardDef
dholeTunnel =
  (treachery "06272" "Dhole Tunnel" TerrorOfTheVale 3)
    { cdCardTraits = singleton Hazard
    }

falseAwakening :: CardDef
falseAwakening =
  (weakness "06275" "False Awakening")
    { cdCardTraits = setFromList [Curse]
    , cdEncounterSet = Just PointOfNoReturn
    , cdEncounterSetQuantity = Just 1
    }

litByDeathFire :: CardDef
litByDeathFire =
  (treachery "06269" "Lit by Death-Fire" PointOfNoReturn 2)
    { cdCardTraits = singleton Hazard
    }

shadowOfAtlachNacha :: CardDef
shadowOfAtlachNacha =
  (treachery "06274" "Shadow of Atlach-Nacha" DescentIntoThePitch 2)
    { cdCardTraits = singleton Curse
    }

tasteOfLifeblood :: CardDef
tasteOfLifeblood =
  (treachery "06268" "Taste of Lifeblood" PointOfNoReturn 2)
    { cdCardTraits = singleton Hazard
    }

unexpectedAmbush :: CardDef
unexpectedAmbush =
  (treachery "06270" "Unexpected Ambush" PointOfNoReturn 2)
    { cdCardTraits = singleton Scheme
    }
