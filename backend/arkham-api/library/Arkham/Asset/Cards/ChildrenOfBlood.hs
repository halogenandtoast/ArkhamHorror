module Arkham.Asset.Cards.ChildrenOfBlood where

import Arkham.Asset.Cards.Import
import Arkham.Trait qualified as Trait

detectiveReynoldsInOverHisHead :: CardDef
detectiveReynoldsInOverHisHead =
  (storyAsset "13029" ("Detective Reynolds" <:> "In Over His Head") 3 RiverOfBlood)
    { cdSkills = [#willpower, #willpower]
    , cdCardTraits = setFromList [Ally, Police]
    , cdUnique = True
    , cdSlots = [#ally]
    }

fangOfZburamoarte :: CardDef
fangOfZburamoarte =
  (storyAsset "13030" "Fang of Zburamoarte" 3 RiverOfBlood)
    { cdSkills = [#combat, #combat]
    , cdCardTraits = setFromList [Item, Weapon, Relic]
    , cdUnique = True
    , cdSlots = [#hand]
    , cdUses = uses Charge 4
    }

sanguineSong :: CardDef
sanguineSong =
  (storyAsset "13066" "Sanguine Song" 3 NewHorizons)
    { cdSkills = [#intellect, #intellect]
    , cdCardTraits = singleton Spell
    , cdSlots = [#arcane]
    , cdUses = uses Charge 4
    }

forgedPermit :: CardDef
forgedPermit =
  (storyAsset "13067" "Forged Permit" 1 NewHorizons)
    { cdSkills = [#wild, #wild]
    , cdCardTraits = setFromList [Item, Trait.Evidence, Illicit]
    }

chosenOfZburamoarteFightingTheHunger :: CardDef
chosenOfZburamoarteFightingTheHunger =
  permanent
    $ (storyAsset_ "13093a" ("Chosen of Zburamoarte" <:> "Fighting the Hunger") BloodMoney)
      { cdCardTraits = setFromList [Condition, Trait.Blight, Trait.Reward]
      , cdUnique = True
      , cdOtherSide = Just "13093b"
      }

chosenOfZburamoarteCompelledToFeed :: CardDef
chosenOfZburamoarteCompelledToFeed =
  permanent
    $ (storyAsset_ "13093b" ("Chosen of Zburamoarte" <:> "Compelled to Feed") BloodMoney)
      { cdCardTraits = setFromList [Condition, Trait.Blight, Trait.Reward]
      , cdUnique = True
      , cdOtherSide = Just "13093a"
      }

charlieKaneKnowsAGuy :: CardDef
charlieKaneKnowsAGuy =
  (storyAsset "13105" ("Charlie Kane" <:> "Knows a Guy") 3 FriendsInLowPlaces)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }
