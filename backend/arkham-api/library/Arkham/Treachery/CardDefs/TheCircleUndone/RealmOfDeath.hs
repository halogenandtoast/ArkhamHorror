module Arkham.Treachery.CardDefs.TheCircleUndone.RealmOfDeath where

import Arkham.Treachery.CardDefs.Import

realmOfTorment :: CardDef
realmOfTorment =
  (treachery "05105" "Realm of Torment" RealmOfDeath 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }

shapesInTheMist :: CardDef
shapesInTheMist =
  surge
    $ (treachery "05106" "Shapes in the Mist" RealmOfDeath 2)
      { cdCardTraits = setFromList [Terror, Spectral]
      }
