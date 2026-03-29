module Arkham.Asset.Cards.TommyMuldoon2 where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

becky2 :: CardDef
becky2 =
  signature "60151"
    $ (asset "60152" ("Becky" <:> "Custom Marlin Model 1894") 2 Neutral)
      { cdCardTraits = setFromList [Item, Weapon, Firearm]
      , cdSkills = [#willpower, #combat, #wild]
      , cdSlots = [#hand, #hand]
      , cdUses = uses Ammo 2
      , cdUnique = True
      }

policeDog :: CardDef
policeDog =
  (asset "60156" "Police Dog" 3 Guardian)
    { cdCardTraits = setFromList [Ally, Creature, Police]
    , cdSkills = [#agility]
    , cdSlots = [#ally]
    }

rookieCop :: CardDef
rookieCop =
  (asset "60157" "Rookie Cop" 3 Guardian)
    { cdCardTraits = setFromList [Ally, Police]
    , cdSkills = [#willpower]
    , cdSlots = [#ally]
    }

serviceRevolver :: CardDef
serviceRevolver =
  (asset "60158" "Service Revolver" 2 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm, Police]
    , cdSkills = [#agility]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 3
    }

protectiveVest :: CardDef
protectiveVest =
  (asset "60159" "Protective Vest" 3 Guardian)
    { cdCardTraits = setFromList [Item, Armor]
    , cdSkills = [#willpower]
    , cdSlots = [#body]
    }

policeDog1 :: CardDef
policeDog1 =
  (asset "60170" "Police Dog" 3 Guardian)
    { cdCardTraits = setFromList [Ally, Creature, Police]
    , cdSkills = [#agility]
    , cdSlots = [#ally]
    , cdLevel = Just 1
    }

m1911Officer2 :: CardDef
m1911Officer2 =
  (asset "60171" "M1911" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#agility, #combat]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 4
    , cdLevel = Just 2
    }

detectiveSherman3 :: CardDef
detectiveSherman3 =
  (asset "60177" ("Detective Sherman" <:> "Eagle Eye") 4 Guardian)
    { cdCardTraits = setFromList [Ally, Detective, Police]
    , cdSkills = [#combat, #intellect, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 3
    }

endurance3 :: CardDef
endurance3 =
  (asset "60178" "Endurance" 2 Guardian)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#agility, #combat]
    , cdKeywords = singleton Keyword.Starting
    , cdLevel = Just 3
    }

protectiveVest4 :: CardDef
protectiveVest4 =
  (asset "60181" "Protective Vest" 3 Guardian)
    { cdCardTraits = setFromList [Item, Armor, Police]
    , cdSkills = [#combat, #wild, #willpower]
    , cdSlots = [#body]
    , cdLevel = Just 4
    }

thompsonSubmachineGun5 :: CardDef
thompsonSubmachineGun5 =
  (asset "60182" "Thompson Submachine Gun" 6 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#combat, #combat]
    , cdSlots = [#hand, #hand]
    , cdUses = uses Ammo 6
    , cdLevel = Just 5
    }
