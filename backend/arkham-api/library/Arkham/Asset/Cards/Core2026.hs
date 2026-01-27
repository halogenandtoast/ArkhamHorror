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

covertOpsInTheShadows :: CardDef
covertOpsInTheShadows =
  signature "12007"
    $ (asset "12008" ("Covert Ops" <:> "In the Shadows") 2 Rogue)
      { cdSkills = [#intellect, #agility, #wild]
      , cdCardTraits = setFromList [Talent, Illicit]
      }

isabellesTwin45sLoadedWithVengeance :: CardDef
isabellesTwin45sLoadedWithVengeance =
  signature "12013"
    $ (asset "12014" ("Isabelle's Twin .45s" <:> "Loaded with Vengeance") 4 Survivor)
      { cdSkills = [#combat, #agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdUnique = True
      , cdUses = uses Ammo 6
      , cdSlots = [#hand, #hand]
      }
