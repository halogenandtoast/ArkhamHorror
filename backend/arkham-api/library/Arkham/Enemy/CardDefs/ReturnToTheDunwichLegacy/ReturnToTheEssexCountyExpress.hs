module Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.ReturnToTheEssexCountyExpress where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

theConductorBeastFromBeyondTheGate :: CardDef
theConductorBeastFromBeyondTheGate =
  doubleSided "51026"
    $ (enemy "51026b" ("The Conductor" <:> "Beast from beyond the Gate") ReturnToTheEssexCountyExpress 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }
