module Arkham.Enemy.CardDefs.ThePathToCarcosa.TheLastKing where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

ashleighClarke :: CardDef
ashleighClarke =
  unique
    $ doubleSided "03069"
    $ (enemy "03069b" ("Ashleigh Clarke" <:> "Songs Die Unheard") TheLastKing 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

constanceDumaine :: CardDef
constanceDumaine =
  unique
    $ doubleSided "03065"
    $ ( enemy
          "03065b"
          ("Constance Dumaine" <:> "A Little Too Sociable")
          TheLastKing
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

dianneDevine :: CardDef
dianneDevine =
  unique
    $ (enemy "03081" ("Dianne Devine" <:> "Mercurial and Mischevious") TheLastKing 1)
      { cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = singleton Keyword.Aloof
      }

ishimaruHaruko :: CardDef
ishimaruHaruko =
  unique
    $ doubleSided "03067"
    $ (enemy "03067b" ("Ishimaru Haruko" <:> "Just Skin and Bones") TheLastKing 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 6
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

jordanPerry :: CardDef
jordanPerry =
  unique
    $ doubleSided "03066"
    $ (enemy "03066b" ("Jordan Perry" <:> "An Imposing Presence") TheLastKing 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 8
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = singleton Keyword.Hunter
      , cdVictoryPoints = Just 0
      }

sebastienMoreau :: CardDef
sebastienMoreau =
  unique
    $ doubleSided "03068"
    $ (enemy "03068b" ("Sebastien Moreau" <:> "Savage Hysteria") TheLastKing 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Monster, Lunatic, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 0
      }
