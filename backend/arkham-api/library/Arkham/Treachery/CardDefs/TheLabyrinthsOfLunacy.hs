module Arkham.Treachery.CardDefs.TheLabyrinthsOfLunacy where

import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

bloodAndRust :: CardDef
bloodAndRust =
  (treachery "70058" "Blood and Rust" TheLabyrinthsOfLunacy 3)
    { cdCardTraits = setFromList [Trait.Trap]
    }

dreadfulMechanism :: CardDef
dreadfulMechanism =
  (treachery "70057" "Dreadful Mechanism" TheLabyrinthsOfLunacy 3)
    { cdCardTraits = setFromList [Trait.Trap]
    }

harvestedPain :: CardDef
harvestedPain =
  (treachery "70061" "Harvested Pain" TheLabyrinthsOfLunacy 3)
    { cdCardTraits = setFromList [Trait.Hex]
    , cdKeywords = setFromList [Keyword.Peril]
    }

paradoxEffect :: CardDef
paradoxEffect =
  (treachery "70060" "Paradox Effect" LabyrinthsOfLunacySingleGroup 3)
    { cdCardTraits = setFromList [Trait.Hazard]
    }

paradoxEffectEpicMultiplayer :: CardDef
paradoxEffectEpicMultiplayer =
  (treachery "70059" "Paradox Effect" LabyrinthsOfLunacyEpicMultiplayer 3)
    { cdCardTraits = setFromList [Trait.Hazard]
    }

poisonousGas :: CardDef
poisonousGas =
  (treachery "70056" "Poisonous Gas" TheLabyrinthsOfLunacy 3)
    { cdCardTraits = setFromList [Trait.Trap, Trait.Poison]
    }

unnaturalWeariness :: CardDef
unnaturalWeariness =
  (treachery "70055" "Unnatural Weariness" TheLabyrinthsOfLunacy 3)
    { cdCardTraits = setFromList [Trait.Injury, Trait.Poison]
    , cdKeywords = setFromList [Keyword.Peril]
    }
