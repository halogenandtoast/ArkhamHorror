module Arkham.Asset.Cards.TheDrownedCity where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

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

oculusMortuum :: CardDef
oculusMortuum =
  (asset "11038" "Oculus Mortuum" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tool, Occult]
    , cdUses = uses Evidence 0
    , cdSlots = [#hand]
    , cdUnique = True
    }

uncannySpecimen :: CardDef
uncannySpecimen =
  (asset "11039" "Uncanny Specimen" 1 Seeker)
    { cdSlots = [#arcane]
    , cdCardTraits = setFromList [Creature, Science]
    , cdKeywords = setFromList [Keyword.Myriad]
    }

lugerP08 :: CardDef
lugerP08 =
  (asset "11050" "Luger P08" 2 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Item, Weapon, Firearm, Illicit]
    , cdSlots = [#hand]
    , cdUses = uses Ammo 2
    }

robertCastaigneHasYourBack :: CardDef
robertCastaigneHasYourBack =
  (asset "11051" ("Robert Castaigne" <:> "Has Your Back") 4 Rogue)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Ally, Veteran]
    , cdSlots = [#ally]
    , cdUnique = True
    }

stringAlong :: CardDef
stringAlong =
  (asset "11052" "String Along" 3 Rogue)
    { cdSkills = [#agility]
    , cdCardTraits = setFromList [Talent]
    }

bloodOfThothLawIncarnate :: CardDef
bloodOfThothLawIncarnate =
  (asset "11065" ("Blood of Thoth" <:> "Law Incarnate") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdSlots = [#arcane]
    }

breathOfTheSleeper :: CardDef
breathOfTheSleeper =
  (asset "11066" "Breath of the Sleeper" 3 Mystic)
    { cdSkills = [#combat]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 9
    , cdSlots = [#arcane]
    }

eyesOfTheDreamer :: CardDef
eyesOfTheDreamer =
  (asset "11067" "Eyes of the Dreamer" 3 Mystic)
    { cdSkills = [#intellect]
    , cdCardTraits = setFromList [Spell]
    , cdUses = uses Charge 9
    , cdSlots = [#arcane]
    }

theGreatWorkDivideAndUnite :: CardDef
theGreatWorkDivideAndUnite =
  permanent
    $ (asset "11068a" ("The Great Work" <:> "Divide and Unite") 0 Mystic)
      { cdCardTraits = setFromList [Pact, Science]
      , cdUnique = True
      , cdDeckRestrictions = [PerDeckLimit 1]
      }

katarinaSojkamissaryFromUlthar :: CardDef
katarinaSojkamissaryFromUlthar =
  (asset "11069" ("Katarina Sojka" <:> "Emissary from Ulthar") 2 Mystic)
    { cdSkills = [#willpower]
    , cdCardTraits = setFromList [Ally, Clairvoyant, Dreamer]
    , cdSlots = [#ally]
    , cdUnique = True
    }
