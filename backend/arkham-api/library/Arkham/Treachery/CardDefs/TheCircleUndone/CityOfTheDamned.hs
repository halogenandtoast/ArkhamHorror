module Arkham.Treachery.CardDefs.TheCircleUndone.CityOfTheDamned where

import Arkham.Treachery.CardDefs.Import

unhallowedLand :: CardDef
unhallowedLand =
  (treachery "54071" "Unhallowed Land" CityOfTheDamned 3)
    { cdCardTraits = setFromList [Curse]
    }

viceAndVillainy :: CardDef
viceAndVillainy =
  (treachery "54070" "Vice and Villainy" CityOfTheDamned 2)
    { cdCardTraits = setFromList [Curse]
    }
