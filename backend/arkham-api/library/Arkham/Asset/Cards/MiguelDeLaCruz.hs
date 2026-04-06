module Arkham.Asset.Cards.MiguelDeLaCruz where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait

miguelsKnapsack :: CardDef
miguelsKnapsack =
  signature "60551"
    $ (asset "60552" "Miguel's Knapsack" 2 Survivor)
      { cdCardTraits = singleton Item
      , cdSkills = [#wild, #wild]
      , cdSlots = [#body]
      , cdUnique = True
      }

danielJameson :: CardDef
danielJameson =
  (asset "60555" ("Daniel Jameson" <:> "Seasoned Hunter") 2 Survivor)
    { cdCardTraits = setFromList [Ally, Hunter]
    , cdSkills = [#agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

huntingDog :: CardDef
huntingDog =
  (asset "60556" "Hunting Dog" 2 Survivor)
    { cdCardTraits = setFromList [Ally, Creature]
    , cdSkills = [#combat]
    , cdSlots = [#ally]
    }

loner :: CardDef
loner =
  (asset "60557" "Loner" 2 Survivor)
    { cdCardTraits = setFromList [Condition]
    , cdSkills = [#willpower]
    , cdLimits = [LimitPerInvestigator 1]
    }

oldCompass :: CardDef
oldCompass =
  (asset "60558" "Old Compass" 2 Survivor)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    }

pocketknife :: CardDef
pocketknife =
  (asset "60559" "Pocketknife" 2 Survivor)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#agility]
    , cdSlots = [#hand]
    }

sameOldThing :: CardDef
sameOldThing =
  (asset "60561" "Same Old Thing" 2 Survivor)
    { cdCardTraits = singleton Condition
    , cdSkills = [#intellect]
    , cdUses = uses Supply 5
    }

extraRations1 :: CardDef
extraRations1 =
  (asset "60570" "Extra Rations" 2 Survivor)
    { cdCardTraits = setFromList [Item, Trait.Supply]
    , cdSkills = [#wild]
    , cdUses = uses Supply 4
    , cdLevel = Just 1
    }

canteen2 :: CardDef
canteen2 =
  (asset "60573" "Canteen" 1 Survivor)
    { cdCardTraits = setFromList [Item, Trait.Supply]
    , cdSkills = [#willpower]
    , cdSlots = [#accessory]
    , cdUses = uses Supply 3
    , cdLevel = Just 2
    }

huntersInstinct2 :: CardDef
huntersInstinct2 =
  (asset "60574" "Hunter's Instinct" 2 Survivor)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#agility, #combat, #wild]
    , cdUses = uses Supply 3
    , cdLimits = [LimitPerInvestigator 1]
    , cdLevel = Just 2
    }

winchesterModel522 :: CardDef
winchesterModel522 =
  (asset "60575" "Winchester Model 52" 3 Survivor)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#agility, #combat]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 2
    , cdLevel = Just 2
    }

levelheaded3 :: CardDef
levelheaded3 =
  (asset "60579" "Levelheaded" 2 Survivor)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#agility, #wild, #willpower]
    , cdKeywords = singleton Keyword.Starting
    , cdLevel = Just 3
    }

longbow3 :: CardDef
longbow3 =
  (asset "60580" "Longbow" 3 Survivor)
    { cdCardTraits = setFromList [Item, Weapon, Ranged]
    , cdSkills = [#agility, #combat, #wild]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 1
    , cdLevel = Just 3
    }
