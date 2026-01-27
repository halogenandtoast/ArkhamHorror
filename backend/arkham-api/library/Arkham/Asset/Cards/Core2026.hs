module Arkham.Asset.Cards.Core2026 where

import Arkham.Asset.Cards.Import

danielasWrenchNewAndImproved :: CardDef
danielasWrenchNewAndImproved =
  signature "12001"
    $ (asset "12002" ("Daniela's Wrench" <:> "New and Improved") 2 Guardian)
      { cdSkills = [#combat, #wild]
      , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
      , cdUnique = True
      , cdSlots = [#hand]
      }
