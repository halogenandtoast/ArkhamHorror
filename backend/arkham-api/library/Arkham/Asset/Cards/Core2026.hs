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

bodyguard :: CardDef
bodyguard =
  (asset "12016" "Bodyguard" 3 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally]
    , cdSlots = [#ally]
    }

endurance :: CardDef
endurance =
  (asset "12017" "Endurance" 2 Guardian)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Talent]
    }

loganHastingsBountyHunter :: CardDef
loganHastingsBountyHunter =
  (asset "12018" ("Logan Hastings" <:> "Bounty Hunter") 4 Guardian)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Hunter]
    , cdSlots = [#ally]
    , cdUnique = True
    }

m1911 :: CardDef
m1911 =
  (asset "12019" "M1911" 3 Guardian)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUnique = True
    , cdUses = uses Ammo 4
    , cdSlots = [#hand]
    }

resilience :: CardDef
resilience =
  (asset "12021" "Resilience" 3 Guardian)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Talent]
    }

bodyguard2 :: CardDef
bodyguard2 =
  (asset "12027" "Bodyguard" 3 Guardian)
    { cdSkills = [#combat, #wild]
    , cdCardTraits = setFromList [Ally]
    , cdSlots = [#ally]
    , cdLevel = Just 2
    }

sledgehammer3 :: CardDef
sledgehammer3 =
  (asset "12028" "Sledgehammer" 3 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Tool, Weapon, Melee]
    , cdSlots = [#hand, #hand]
    , cdLevel = Just 3
    }

winchesterModel125 :: CardDef
winchesterModel125 =
  (asset "12029" "Winchester Model 12" 4 Guardian)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 3
    , cdLevel = Just 5
    }

sharpRhetoric :: CardDef
sharpRhetoric =
  (asset "12035" "Sharp Rhetoric" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent]
    }

silverTongue :: CardDef
silverTongue =
  (asset "12047" "Silver Tongue" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Talent]
    }

levelheaded :: CardDef
levelheaded =
  (asset "12076" "Levelheaded" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Talent]
    }

fedora :: CardDef
fedora =
  (asset "12087" "Fedora" 2 Neutral)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Apparel, Headwear]
    , cdSlots = [#head]
    }

theGoldBug :: CardDef
theGoldBug =
  (basicWeakness "12098" "The Gold Bug")
    { cdCardTraits = setFromList [Item, Relic, Alchemy, Cursed]
    , cdSlots = [#accessory]
    }
