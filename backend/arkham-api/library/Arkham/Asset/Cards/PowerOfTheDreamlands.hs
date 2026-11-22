module Arkham.Asset.Cards.PowerOfTheDreamlands where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

{- | A dreaming card has NO printed resource cost (@cost: -5@ in the card data);
it is paid for with dreams instead. Keep the @cdCost = Nothing@ and the
'Keyword.Dreaming' together so neither can be set without the other.
-}
dreaming :: Int -> CardDef -> CardDef
dreaming n cd = cd {cdCost = Nothing, cdKeywords = insertSet (Keyword.Dreaming n) (cdKeywords cd)}

knightOfCelephais :: CardDef
knightOfCelephais =
  dreaming 4
    $ (asset "14002" "Knight of Celephaïs" 0 Guardian)
      { cdSkills = [#intellect, #combat]
      , cdCardTraits = setFromList [Ally, Soldier, Dreamlands]
      , cdSlots = [#ally]
      }

knightsLongsword :: CardDef
knightsLongsword =
  dreaming 3
    $ (asset "14003" "Knight's Longsword" 0 Guardian)
      { cdSkills = [#combat, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Dreamlands]
      , cdSlots = [#hand]
      }

somniumShotcaster4 :: CardDef
somniumShotcaster4 =
  dreaming 2
    $ fast
    $ (asset "14013" "Somnium Shotcaster" 0 Guardian)
      { cdSkills = [#combat, #agility]
      , cdCardTraits = setFromList [Item, Weapon, Firearm, Dreamlands]
      , cdUses = uses Ammo 4
      , cdSlots = [#hand]
      , cdLevel = Just 4
      }

grimoireOfAges :: CardDef
grimoireOfAges =
  dreaming 2
    $ (asset "14016" "Grimoire of Ages" 0 Seeker)
      { cdSkills = [#willpower, #intellect]
      , cdCardTraits = setFromList [Item, Tome, Dreamlands]
      , cdUses = uses Secret 3
      , cdSlots = [#hand]
      }

lunarCharts :: CardDef
lunarCharts =
  dreaming 3
    $ (asset "14017" "Lunar Charts" 0 Seeker)
      { cdSkills = [#willpower, #intellect]
      , cdCardTraits = setFromList [Item, Dreamlands]
      , cdUses = uses Secret 3
      , cdSlots = [#hand]
      }

idolOfBokrug5 :: CardDef
idolOfBokrug5 =
  (asset "14028" ("Idol of Bokrug" <:> "Remnant of Ib") 2 Seeker)
    { cdSkills = [#willpower, #intellect, #wild, #wild]
    , cdCardTraits = setFromList [Item, Relic, Dreamlands]
    , cdUnique = True
    , cdSlots = [#accessory]
    , cdLevel = Just 5
    }

dalkura :: CardDef
dalkura =
  dreaming 3
    $ (asset "14030" ("Dalkura" <:> "Many-Pockets") 0 Rogue)
      { cdSkills = [#intellect, #agility]
      , cdCardTraits = setFromList [Ally, Corsair, Dreamlands]
      , cdUnique = True
      , cdSlots = [#ally]
      }

bladeOfTheSkySea3 :: CardDef
bladeOfTheSkySea3 =
  dreaming 2
    $ (asset "14040" "Blade of the Sky-Sea" 0 Rogue)
      { cdSkills = [#combat, #agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Dreamlands]
      , cdSlots = [#hand]
      , cdLevel = Just 3
      }

enchantingFife :: CardDef
enchantingFife =
  (asset "14043" "Enchanting Fife" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic, Dreamlands]
    , cdUses = uses Charge 3
    , cdSlots = [#hand]
    }

lukeRobinson :: CardDef
lukeRobinson =
  dreaming 3
    $ (asset "14045" ("Luke Robinson" <:> "Wanderer in Dreams") 0 Mystic)
      { cdSkills = [#willpower]
      , cdCardTraits = setFromList [Ally, Dreamer, Dreamlands]
      , cdUnique = True
      , cdSlots = [#ally]
      }

sigilOfThalarion2 :: CardDef
sigilOfThalarion2 =
  permanent
    $ (asset "14052" ("Sigil of Thalarion" <:> "Sign of a Thousand Wonders") 0 Mystic)
      { cdCardTraits = setFromList [Pact, Sigil, Dreamlands]
      , cdUnique = True
      , cdLimits = [LimitPerTrait Sigil 1]
      , cdDeckRestrictions = [PerDeckLimit 1]
      , cdLevel = Just 2
      }

lunarSight3 :: CardDef
lunarSight3 =
  dreaming 3
    $ (asset "14054" "Lunar Sight" 0 Mystic)
      { cdSkills = [#willpower, #intellect, #wild]
      , cdCardTraits = setFromList [Spell, Dreamlands]
      , cdUses = uses Charge 4
      , cdSlots = [#arcane]
      , cdLevel = Just 3
      }

realityFracture3 :: CardDef
realityFracture3 =
  dreaming 3
    $ (asset "14055" "Reality Fracture" 0 Mystic)
      { cdSkills = [#willpower, #combat, #wild]
      , cdCardTraits = setFromList [Spell, Dreamlands]
      , cdUses = uses Charge 4
      , cdSlots = [#arcane]
      , cdLevel = Just 3
      }

explorersSextant :: CardDef
explorersSextant =
  dreaming 2
    $ (asset "14057" "Explorer's Sextant" 0 Survivor)
      { cdSkills = [#willpower, #intellect]
      , cdCardTraits = setFromList [Item, Tool, Dreamlands]
      , cdSlots = [#hand]
      }

lashOfUlthar2 :: CardDef
lashOfUlthar2 =
  dreaming 2
    $ (asset "14067" "Lash of Ulthar" 0 Survivor)
      { cdSkills = [#combat, #agility, #wild]
      , cdCardTraits = setFromList [Item, Weapon, Melee, Dreamlands]
      , cdSlots = [#hand]
      , cdLevel = Just 2
      }

staffOfAtal3 :: CardDef
staffOfAtal3 =
  (asset "14070" ("Staff of Atal" <:> "Hand of Ulthar") 2 Survivor)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Item, Relic, Dreamlands]
    , cdUnique = True
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }
