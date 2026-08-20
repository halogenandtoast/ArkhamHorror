module Arkham.Treachery.CardDefs.VileExperiments where

import Arkham.Treachery.CardDefs.Import

harvestedBrain :: CardDef
harvestedBrain =
  (treachery "84038" "Harvested Brain" VileExperiments 1)
    { cdCardTraits = setFromList [Ancient, Science]
    , cdRevelation = NoRevelation
    }

morbidAwareness :: CardDef
morbidAwareness =
  (treachery "84039" "Morbid Awareness" VileExperiments 3)
    { cdCardTraits = singleton Hazard
    }
