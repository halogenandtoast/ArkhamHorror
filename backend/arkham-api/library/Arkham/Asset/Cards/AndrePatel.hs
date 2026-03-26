module Arkham.Asset.Cards.AndrePatel where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

centerStage :: CardDef
centerStage =
  (asset "60357" "Center Stage" 3 Rogue)
    { cdCardTraits = singleton Talent
    , cdLimits = [LimitPerInvestigator 1]
    , cdUses = uses Renown 3
    }

fame :: CardDef
fame =
  (asset "60358" "Fame" 1 Rogue)
    { cdCardTraits = singleton Condition
    , cdSkills = [#agility]
    , cdUses = uses Renown 4
    }

theGrapevine :: CardDef
theGrapevine =
  (asset "60359" "The Grapevine" 2 Rogue)
    { cdCardTraits = singleton Connection
    , cdSkills = [#agility]
    , cdUses = uses Rumor 3
    }

extravagantRing :: CardDef
extravagantRing =
  (asset "60360" "Extravagant Ring" 1 Rogue)
    { cdCardTraits = setFromList [Item, Charm]
    , cdSkills = [#intellect]
    , cdSlots = [#accessory]
    , cdUses = uses Renown 3
    }

marcusSengstacke :: CardDef
marcusSengstacke =
  (asset "60362" ("Marcus Sengstacke" <:> "Wealthy Benefactor") 3 Rogue)
    { cdCardTraits = setFromList [Ally, Patron]
    , cdSkills = [#intellect, #willpower]
    , cdSlots = [#ally]
    , cdUnique = True
    }

polishedCane :: CardDef
polishedCane =
  (asset "60363" "Polished Cane" 3 Rogue)
    { cdCardTraits = setFromList [Item, Weapon, Melee]
    , cdSkills = [#agility]
    , cdSlots = [#hand]
    }

theGrapevine2 :: CardDef
theGrapevine2 =
  (asset "60374" "The Grapevine" 2 Rogue)
    { cdCardTraits = singleton Connection
    , cdSkills = [#agility, #intellect]
    , cdUses = uses Rumor 3
    , cdLevel = Just 2
    }

marcusSengstacke2 :: CardDef
marcusSengstacke2 =
  (asset "60375" ("Marcus Sengstacke" <:> "Wealthy Benefactor") 2 Rogue)
    { cdCardTraits = setFromList [Ally, Patron]
    , cdSkills = [#intellect, #willpower]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdLevel = Just 2
    }

silverTongue3 :: CardDef
silverTongue3 =
  (asset "60380" "Silver Tongue" 2 Rogue)
    { cdCardTraits = singleton Talent
    , cdSkills = [#intellect, #agility, #wild]
    , cdKeywords = singleton Keyword.Starting
    , cdLevel = Just 3
    }
