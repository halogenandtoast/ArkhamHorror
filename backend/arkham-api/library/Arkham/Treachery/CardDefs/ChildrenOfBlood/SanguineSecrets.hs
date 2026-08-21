module Arkham.Treachery.CardDefs.ChildrenOfBlood.SanguineSecrets where

import Arkham.Treachery.CardDefs.Import

morbidRituals :: CardDef
morbidRituals =
  (treachery "13114" "Morbid Rituals" SanguineSecrets 2)
    { cdCardTraits = singleton Hex
    }
