module Arkham.Treachery.CardDefs.TheCircleUndone.InTheClutchesOfChaos where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

chaosManifest :: CardDef
chaosManifest =
  (treachery "05306" "Chaos Manifest" InTheClutchesOfChaos 3)
    { cdCardTraits = singleton Power
    }

primordialGateway :: CardDef
primordialGateway =
  (treachery "05307" "Primordial Gateway" InTheClutchesOfChaos 2)
    { cdCardTraits = singleton Power
    }

secretsOfTheBeyond :: CardDef
secretsOfTheBeyond =
  (treachery "05310" "Secrets of the Beyond" SecretsOfTheUniverse 2)
    { cdCardTraits = singleton Hex
    }

terrorUnleashed :: CardDef
terrorUnleashed =
  (treachery "05308" "Terror Unleashed" InTheClutchesOfChaos 3)
    { cdCardTraits = singleton Curse
    }

toilAndTrouble :: CardDef
toilAndTrouble =
  (treachery "05312" "Toil and Trouble" MusicOfTheDamned 2)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Peril
    }
