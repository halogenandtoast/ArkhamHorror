module Arkham.Asset.Cards.ChildrenOfBlood where

import Arkham.Asset.Cards.Import

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

charlieKaneKnowsAGuy :: CardDef
charlieKaneKnowsAGuy =
  (storyAsset "13105" ("Charlie Kane" <:> "Knows a Guy") 3 FriendsInLowPlaces)
    { cdSkills = [#agility, #agility]
    , cdCardTraits = setFromList [Ally, Patron]
    , cdUnique = True
    , cdSlots = [#ally]
    }
