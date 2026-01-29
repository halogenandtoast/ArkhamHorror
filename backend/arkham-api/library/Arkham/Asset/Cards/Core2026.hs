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

dorothySimmonsStraightAStudent :: CardDef
dorothySimmonsStraightAStudent =
  (asset "12030" ("Dorothy Simmons" <:> "Straight-A Student") 3 Seeker)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSlots = [#ally]
    , cdUnique = True
    }

localMap :: CardDef
localMap =
  (asset "12033" "Local Map" 3 Seeker)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

sharpRhetoric :: CardDef
sharpRhetoric =
  (asset "12035" "Sharp Rhetoric" 2 Seeker)
    { cdSkills = [#willpower, #intellect]
    , cdCardTraits = setFromList [Talent]
    }

mysteriousGrimoire2 :: CardDef
mysteriousGrimoire2 =
  (asset "12040" "Mysterious Grimoire" 3 Seeker)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tome]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

m1903Hammerless :: CardDef
m1903Hammerless =
  (asset "12045" "M1903 Hammerless" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdUses = uses Ammo 4
    , cdSlots = [#hand]
    }

olivierBishopHaughtyArtCollector :: CardDef
olivierBishopHaughtyArtCollector =
  (asset "12046" ("Olivier Bishop" <:> "Haughty Art \"Collector\"") 4 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Ally, Criminal, Socialite]
    , cdSlots = [#ally]
    , cdUnique = True
    }

silverTongue :: CardDef
silverTongue =
  (asset "12047" "Silver Tongue" 2 Rogue)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Talent]
    }

stickyFingers :: CardDef
stickyFingers =
  (asset "12048" "Sticky Fingers" 1 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent]
    , cdLimits = [LimitPerInvestigator 1]
    }

stickyFingers2 :: CardDef
stickyFingers2 =
  fast
    (asset "12054" "Sticky Fingers" 1 Rogue)
      { cdSkills = [#agility, #agility]
      , cdCardTraits = setFromList [Talent]
      , cdLimits = [LimitPerInvestigator 1]
      , cdLevel = Just 2
      }

cloakOfResonance :: CardDef
cloakOfResonance =
  (asset "12058" "Cloak of Resonance" 3 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Clothing, Alchemy]
    , cdSlots = [#body]
    }

cosmicFlame :: CardDef
cosmicFlame =
  (asset "12059" "Cosmic Flame" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

jimCulverHauntedMusician :: CardDef
jimCulverHauntedMusician =
  (asset "12060" ("Jim Culver" <:> "Haunted Musician") 4 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Performer]
    , cdSlots = [#ally]
    , cdUnique = True
    }

luckyCharm :: CardDef
luckyCharm =
  (asset "12061" "Lucky Charm" 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Charm, Blessed]
    , cdSlots = [#accessory]
    , cdUses = uses Charge 4
    }

secondSight :: CardDef
secondSight =
  (asset "12062" "Second Sight" 4 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdSlots = [#arcane]
    , cdUses = uses Charge 3
    }

alekseySaburovAlwaysOnTheMend :: CardDef
alekseySaburovAlwaysOnTheMend =
  (asset "12072" ("Aleksey Saburov" <:> "Always on the Mend") 3 Survivor)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally]
    , cdSlots = [#ally]
    , cdUnique = True
    }

huntersInstinct :: CardDef
huntersInstinct =
  (asset "12074" "Hunter's Instinct" 2 Survivor)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Talent]
    , cdLimits = [LimitPerInvestigator 1]
    , cdUses = uses Supply 3
    }

jumpsuit :: CardDef
jumpsuit =
  (asset "12075" "Jumpsuit" 1 Survivor)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Clothing]
    , cdSlots = [#body]
    }

levelheaded :: CardDef
levelheaded =
  (asset "12076" "Levelheaded" 2 Survivor)
    { cdSkills = [#willpower, #agility]
    , cdCardTraits = setFromList [Talent]
    }

oldCompass2 :: CardDef
oldCompass2 =
  (asset "12083" "Old Compass" 2 Survivor)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = setFromList [Item, Tool]
    , cdSlots = [#hand]
    , cdLevel = Just 2
    }

meatCleaver3 :: CardDef
meatCleaver3 =
  (asset "12085" "Meat Cleaver" 2 Survivor)
    { cdSkills = [#willpower, #combat, #wild]
    , cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSlots = [#hand]
    , cdLevel = Just 3
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
