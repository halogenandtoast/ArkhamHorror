module Arkham.Treachery.CardDefs.ReturnToNightOfTheZealot.ReturnToTheDevourerBelow where

import Arkham.Treachery.CardDefs.Import

umordhothsHunger :: CardDef
umordhothsHunger =
  (treachery "50037" "Umôrdhoth's Hunger" ReturnToTheDevourerBelow 2)
    { cdCardTraits = setFromList [Power]
    }

vaultOfEarthlyDemise :: CardDef
vaultOfEarthlyDemise =
  (treachery "50032b" "Vault of Earthly Demise" ReturnToTheDevourerBelow 1)
    { cdCardTraits = setFromList [Eldritch, Otherworld]
    , cdOtherSide = Just "50032"
    , cdDoubleSided = True
    }
