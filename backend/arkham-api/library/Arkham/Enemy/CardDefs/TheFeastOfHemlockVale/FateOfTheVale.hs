module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.FateOfTheVale where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cosmicEmissaryTheAbyss :: CardDef
cosmicEmissaryTheAbyss =
  doubleSided "10662b"
    $ (enemy "10662a" ("Cosmic Emissary" <:> "The Abyss") FateOfTheVale 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

cosmicEmissaryTheAbyssShattered :: CardDef
cosmicEmissaryTheAbyssShattered =
  doubleSided "10662a"
    $ (enemy "10662b" ("Cosmic Emissary" <:> "The Abyss") FateOfTheVale 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 10
      , cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 5
      }

cosmicEmissaryTheBrilliance :: CardDef
cosmicEmissaryTheBrilliance =
  doubleSided "10665b"
    $ (enemy "10665a" ("Cosmic Emissary" <:> "The Brilliance") FateOfTheVale 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

cosmicEmissaryTheBrillianceShattered :: CardDef
cosmicEmissaryTheBrillianceShattered =
  doubleSided "10665a"
    $ (enemy "10665b" ("Cosmic Emissary" <:> "The Brilliance") FateOfTheVale 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 10
      , cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Elusive, Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

cosmicEmissaryTheMiasma :: CardDef
cosmicEmissaryTheMiasma =
  doubleSided "10664b"
    $ (enemy "10664a" ("Cosmic Emissary" <:> "The Miasma") FateOfTheVale 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Alert]
      }

cosmicEmissaryTheMiasmaShattered :: CardDef
cosmicEmissaryTheMiasmaShattered =
  doubleSided "10664a"
    $ (enemy "10664b" ("Cosmic Emissary" <:> "The Miasma") FateOfTheVale 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 10
      , cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter, Keyword.Alert]
      , cdVictoryPoints = Just 1
      }

cosmicEmissaryThePhantasm :: CardDef
cosmicEmissaryThePhantasm =
  doubleSided "10663b"
    $ (enemy "10663a" ("Cosmic Emissary" <:> "The Phantasm") FateOfTheVale 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Emissary, Colour, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
      }

cosmicEmissaryThePhantasmShattered :: CardDef
cosmicEmissaryThePhantasmShattered =
  doubleSided "10663a"
    $ (enemy "10663b" ("Cosmic Emissary" <:> "The Phantasm") FateOfTheVale 1)
      { cdSanityDamage = sanityDamage 3
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 10
      , cdCardTraits = setFromList [Emissary, Shattered, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

crystalMimic :: CardDef
crystalMimic =
  (enemy "10671" "Crystal Mimic" FateOfTheVale 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evadeX
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Colour]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    }
