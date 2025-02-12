module Arkham.Asset.Cards.TheDrownedCity where

import Arkham.Asset.Cards.Import

bookOfVerseUnCommonplaceBook :: CardDef
bookOfVerseUnCommonplaceBook =
  signature "11004"
    $ (asset "11005" ("Book of Verse" <:> "Un-Commonplace Book") 2 Neutral)
      { cdCardTraits = setFromList [Item, Tome]
      , cdUnique = True
      , cdUses = uses Inspiration 1
      }
