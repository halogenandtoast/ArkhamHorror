module Arkham.Asset.Cards.MarieLambeau2 where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

grandMeresCharm :: CardDef
grandMeresCharm =
  signature "60451"
    $ (asset "60452" "Grand-M\232re's Charm" 2 Mystic)
      { cdCardTraits = setFromList [Item, Charm, Occult, Blessed]
      , cdSkills = [#willpower, #willpower, #wild]
      , cdSlots = [#accessory]
      , cdUnique = True
      }

offeringBowl :: CardDef
offeringBowl =
  (asset "60456" "Offering Bowl" 1 Mystic)
    { cdCardTraits = setFromList [Item, Occult]
    , cdSkills = [#wild]
    , cdUses = uses Offering 3
    }

bloodstone :: CardDef
bloodstone =
  (asset "60457" "Bloodstone" 2 Mystic)
    { cdCardTraits = setFromList [Item, Charm, Cursed]
    , cdSkills = [#willpower]
    , cdSlots = [#accessory]
    }

sacrificialDoll :: CardDef
sacrificialDoll =
  (asset "60458" "Sacrificial Doll" 3 Mystic)
    { cdCardTraits = setFromList [Item, Charm, Occult]
    , cdSkills = [#combat, #willpower]
    , cdSlots = [#hand]
    }

shadowmeld :: CardDef
shadowmeld =
  (asset "60461" "Shadowmeld" 2 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#agility]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

ceremonialRobes1 :: CardDef
ceremonialRobes1 =
  (asset "60470" "Ceremonial Robes" 1 Mystic)
    { cdCardTraits = setFromList [Item, Clothing]
    , cdSkills = [#wild]
    , cdSlots = [#body]
    , cdLevel = Just 1
    }

dreadCurseOfAzathoth3 :: CardDef
dreadCurseOfAzathoth3 =
  (asset "60474" "Dread Curse of Azathoth" 2 Mystic)
    { cdCardTraits = setFromList [Spell, Cursed]
    , cdSkills = [#combat, #willpower]
    , cdSlots = [#arcane]
    , cdExceptional = True
    , cdLevel = Just 3
    }

ritualDagger3 :: CardDef
ritualDagger3 =
  (asset "60475" "Ritual Dagger" 3 Mystic)
    { cdCardTraits = setFromList [Item, Weapon, Melee, Cursed]
    , cdSkills = [#combat, #wild, #willpower]
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }

spiritualIntuition3 :: CardDef
spiritualIntuition3 =
  (asset "60476" "Spiritual Intuition" 2 Mystic)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#combat, #wild, #willpower]
    , cdKeywords = singleton Keyword.Starting
    , cdLevel = Just 3
    }

arcaneExperience4 :: CardDef
arcaneExperience4 =
  permanent
    $ (asset "60478" "Arcane Experience" 0 Mystic)
      { cdCardTraits = setFromList [Condition]
      , cdDeckRestrictions = [PerDeckLimit 1]
      , cdLevel = Just 4
      }

jimCulver4 :: CardDef
jimCulver4 =
  (asset "60479" ("Jim Culver" <:> "Haunted Musician") 4 Mystic)
    { cdCardTraits = setFromList [Ally, Performer]
    , cdSkills = [#willpower, #willpower]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 4
    }

secondSight5 :: CardDef
secondSight5 =
  (asset "60481" "Second Sight" 4 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#intellect, #wild, #willpower]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    , cdLevel = Just 5
    }

shadowmeld5 :: CardDef
shadowmeld5 =
  (asset "60482" "Shadowmeld" 2 Mystic)
    { cdCardTraits = setFromList [Spell]
    , cdSkills = [#agility, #wild, #willpower]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 5
    , cdLevel = Just 5
    }
