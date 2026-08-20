module Arkham.Enemy.CardDefs.ThePathToCarcosa.Hauntings where

import Arkham.Enemy.CardDefs.Import

poltergeist :: CardDef
poltergeist =
  (enemy "03093" "Poltergeist" Hauntings 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    }
