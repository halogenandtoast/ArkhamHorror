module Arkham.Enemy.CardDefs.GuardiansOfTheAbyss.BrotherhoodOfTheBeast where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

drLaylaElMasri :: CardDef
drLaylaElMasri =
  unique
    $ doubleSided "83031b"
    $ (enemy "83031a" ("Dr. Layla El Masri" <:> "Hieratic Translator") BrotherhoodOfTheBeast 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

drWentworthMoore :: CardDef
drWentworthMoore =
  unique
    $ doubleSided "83032b"
    $ (enemy "83032a" ("Dr. Wentworth Moore" <:> "Dark Supplicant") BrotherhoodOfTheBeast 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

farid :: CardDef
farid =
  unique
    $ doubleSided "83034b"
    $ (enemy "83034a" ("Farid" <:> "Seedy Salesman") BrotherhoodOfTheBeast 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

nadiaNimr :: CardDef
nadiaNimr =
  unique
    $ doubleSided "83033b"
    $ (enemy "83033a" ("Nadia Nimr" <:> "Priestess of the Beast") BrotherhoodOfTheBeast 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

nassor :: CardDef
nassor =
  unique
    $ doubleSided "83035b"
    $ (enemy "83035a" ("Nassor" <:> "Brotherhood Operative") BrotherhoodOfTheBeast 1)
      { cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

professorNathanielTaylor :: CardDef
professorNathanielTaylor =
  unique
    $ doubleSided "83036b"
    $ (enemy "83036a" ("Professor Nathaniel Taylor" <:> "Keeper of Antiquities") BrotherhoodOfTheBeast 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }
