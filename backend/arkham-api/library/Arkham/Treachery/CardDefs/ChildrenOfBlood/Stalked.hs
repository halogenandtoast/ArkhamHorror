module Arkham.Treachery.CardDefs.ChildrenOfBlood.Stalked where

import Arkham.Treachery.CardDefs.Import

inTheShadows :: CardDef
inTheShadows =
  (treachery "13115" "In the Shadows" Stalked 2)
    { cdCardTraits = singleton Scheme
    }

voicesInTheNight :: CardDef
voicesInTheNight =
  (treachery "13116" "Voices in the Night" Stalked 3)
    { cdCardTraits = singleton Terror
    }
