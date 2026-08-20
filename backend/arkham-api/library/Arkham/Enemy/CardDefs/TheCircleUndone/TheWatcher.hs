module Arkham.Enemy.CardDefs.TheCircleUndone.TheWatcher where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theSpectralWatcher :: CardDef
theSpectralWatcher =
  unique
    $ (enemy "05086" ("The Spectral Watcher" <:> "You Are Its Prey") TheWatcher 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [AncientOne, Spectral, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      }
