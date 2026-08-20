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
