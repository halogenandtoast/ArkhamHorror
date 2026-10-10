module Arkham.Homebrew.AgesUnwound.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set

aforgomonsBlade :: CardDef
aforgomonsBlade =
  (storyAsset ":ages-unwound:041" "Aforgomon's Blade" 5 Set.TheMyriadGentleman)
    { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSkills = [#combat, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdUses = uses Charge 3
    }

chronalAtlas :: CardDef
chronalAtlas =
  (storyAsset ":ages-unwound:136" ("Chronal Atlas" <:> "Secrets of the Future") 3 Set.Missions)
    { cdCardTraits = setFromList [Item, Tome]
    , cdSkills = [#intellect, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdUses = uses Secret 3
    }

forestallFate :: CardDef
forestallFate =
  fast
    $ (storyAsset ":ages-unwound:137" ("Forestall Fate" <:> "Harnessed Anomaly") 2 Set.Missions)
      { cdCardTraits = singleton Spell
      , cdSkills = [#willpower, #wild]
      , cdSlots = [#arcane]
      , cdUses = uses Charge 4
      }

ionianPendant :: CardDef
ionianPendant =
  (storyAsset ":ages-unwound:138" ("Ionian Pendant" <:> "Fulcrum of Fate") 3 Set.Missions)
    { cdCardTraits = setFromList [Item, Relic]
    , cdSkills = [#combat, #wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    }

wingsOfDamakairon :: CardDef
wingsOfDamakairon =
  fast
    $ (storyAsset ":ages-unwound:139" ("Wings of Damakairon" <:> "A Parting Gift") 2 Set.Missions)
      { cdCardTraits = setFromList [Spell, Blessed]
      , cdSkills = [#agility, #wild]
      , cdSlots = [#body]
      , cdUnique = True
      }

distantEntity :: CardDef
distantEntity =
  otherSideIs ":ages-unwound:144b"
    $ (encounterAsset_ ":ages-unwound:144" "Distant Entity" Set.Missions)
      { cdCardTraits = singleton AncientOne
      }

unstableWarding :: CardDef
unstableWarding =
  otherSideIs ":ages-unwound:231"
    $ (encounterAsset_ ":ages-unwound:231b" "Unstable Warding" Set.NightOfTheRitual)
      { cdCardTraits = singleton Spell
      }
