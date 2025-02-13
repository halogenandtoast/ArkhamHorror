module Arkham.Asset.Cards.TheDrownedCity where

import Arkham.Asset.Cards.Import

bookOfVerseUnCommonplaceBook :: CardDef
bookOfVerseUnCommonplaceBook =
  signature "11004"
    $ (asset "11005" ("Book of Verse" <:> "Un-Commonplace Book") 2 Neutral)
      { cdCardTraits = setFromList [Item, Tome]
      , cdSkills = [#wild, #wild]
      , cdUnique = True
      , cdUses = uses Inspiration 1
      }

oculaObscuraEsotericEyepiece :: CardDef
oculaObscuraEsotericEyepiece =
  (asset "11009" ("Ocula Obscura" <:> "Esoteric Eyepiece") 3 Neutral)
    { cdCardTraits = setFromList [Item, Tool, Science]
    , cdSkills = [#willpower, #intellect, #wild]
    , cdUnique = True
    , cdDeckRestrictions = [Signature "11007", Signature "11008"]
    , cdSlots = [#accessory]
    }

violaCase :: CardDef
violaCase =
  signature "11011"
    $ (asset "11012" "\"Viola\" Case" 2 Neutral)
      { cdCardTraits = setFromList [Item, Illicit]
      , cdSkills = [#willpower, #combat, #agility, #wild]
      , cdSlots = [#accessory]
      }

theBookOfWarSunTzusLegacy :: CardDef
theBookOfWarSunTzusLegacy =
  (asset "11020" ("The Book of War" <:> "Sun Tzu's Legacy") 4 Guardian)
    { cdCardTraits = setFromList [Item, Tome]
    , cdSkills = [#intellect]
    , cdUnique = True
    , cdUses = uses Secret 3
    , cdSlots = [#hand]
    }

crowbar :: CardDef
crowbar =
  (asset "11021" "Crowbar" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Tool, Melee]
    , cdSkills = [#intellect, #combat]
    , cdSlots = [#hand]
    }

remingtonModel1858 :: CardDef
remingtonModel1858 =
  (asset "11022" "Remington Model 1858" 3 Guardian)
    { cdCardTraits = setFromList [Item, Weapon, Firearm]
    , cdSkills = [#agility]
    , cdUses = uses Ammo 2
    , cdSlots = [#hand]
    }

altonOConnellGhostHunter :: CardDef
altonOConnellGhostHunter =
  (asset "11033" ("Alton O'Connell" <:> "Ghost Hunter") 3 Seeker)
    { cdCardTraits = setFromList [Ally, Detective, Reporter]
    , cdSkills = [#intellect, #agility]
    , cdSlots = [#ally]
    , cdUnique = True
    }

artisticInspiration :: CardDef
artisticInspiration =
  (asset "11034" "Artistic Inspiration" 2 Seeker)
    { cdCardTraits = setFromList [Talent]
    , cdSkills = [#wild]
    , cdLimits = [LimitPerInvestigator 1]
    , cdUses = uses Inspiration 1
    }

dialOfAncientsUnidentified :: CardDef
dialOfAncientsUnidentified =
  (asset "11035" ("Dial of Ancients" <:> "Unidentified") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSkills = [#intellect]
    , cdUses = uses Charge 0
    , cdSlots = [#accessory]
    , cdDeckRestrictions = [PerDeckLimit 1]
    , cdUnique = True
    }

forensicKit :: CardDef
forensicKit =
  (asset "11036" "Forensic Kit" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tool, Science]
    , cdSkills = [#intellect, #agility]
    , cdUses = uses Supply 4
    , cdSlots = [#hand]
    }

mortarAndPestle :: CardDef
mortarAndPestle =
  (asset "11037" "Mortar and Pestle" 2 Seeker)
    { cdCardTraits = setFromList [Item, Tool, Science]
    , cdSkills = [#willpower]
    , cdUses = uses Resource 0
    }
