module Arkham.Treachery.CardDefs.TheFeastOfHemlockVale.Myconids where

import Arkham.Treachery.CardDefs.Import

psychotropicSpores :: CardDef
psychotropicSpores =
  (treachery "10740" "Psychotropic Spores" Myconids 2)
    { cdCardTraits = setFromList [Hazard, Flora]
    }
