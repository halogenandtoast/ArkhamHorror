module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

aquaticAbomination :: CardDef
aquaticAbomination =
  (enemy "07332" "Aquatic Abomination" IntoTheMaelstrom 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Abomination]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

dagonAwakenedAndEnragedIntoTheMaelstrom :: CardDef
dagonAwakenedAndEnragedIntoTheMaelstrom =
  doubleSided "07330"
    $ (enemy "07330b" ("Dagon" <:> "Awakened and Enraged") IntoTheMaelstrom 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 7
      , cdEvade = evade 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdUnique = True
      }

dagonDeepInSlumberIntoTheMaelstrom :: CardDef
dagonDeepInSlumberIntoTheMaelstrom =
  doubleSided "07330b"
    $ (enemy "07330" ("Dagon" <:> "Deep in Slumber") IntoTheMaelstrom 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdUnique = True
      }

dagonsBrood :: CardDef
dagonsBrood =
  (enemy "07333" "Dagon's Brood" IntoTheMaelstrom 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hydraAwakenedAndEnraged :: CardDef
hydraAwakenedAndEnraged =
  doubleSided "07331"
    $ (enemy "07331b" ("Hydra" <:> "Awakened and Enraged") IntoTheMaelstrom 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 7
      , cdEvade = evade 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdUnique = True
      }

hydraDeepInSlumber :: CardDef
hydraDeepInSlumber =
  doubleSided "07331b"
    $ (enemy "07331" ("Hydra" <:> "Deep in Slumber") IntoTheMaelstrom 1)
      { cdCardTraits = setFromList [AncientOne, Elite]
      , cdUnique = True
      }

hydrasBrood :: CardDef
hydrasBrood =
  (enemy "07334" "Hydra's Brood" IntoTheMaelstrom 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
