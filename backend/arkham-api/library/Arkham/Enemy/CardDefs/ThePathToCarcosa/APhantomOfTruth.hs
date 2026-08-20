module Arkham.Enemy.CardDefs.ThePathToCarcosa.APhantomOfTruth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

stealthyByakhee :: CardDef
stealthyByakhee =
  (enemy "03222" "Stealthy Byakhee" APhantomOfTruth 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Byakhee]
    , cdKeywords = singleton Keyword.Hunter
    }

theOrganistDrapedInMystery :: CardDef
theOrganistDrapedInMystery =
  unique
    $ doubleSided "03221a"
    $ (enemy "03221b" ("The Organist" <:> "Draped in Mystery") APhantomOfTruth 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdCardTraits = setFromList [Humanoid, Avatar, Elite]
      , cdKeywords = singleton Keyword.Aloof
      }

theOrganistHopelessIDefiedHim :: CardDef
theOrganistHopelessIDefiedHim =
  unique
    $ doubleSided "03221b"
    $ ( enemy
          "03221a"
          ("The Organist" <:> "Hopeless, I Defied Him")
          APhantomOfTruth
          1
      )
      { cdSanityDamage = sanityDamage 3
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Avatar, Elite]
      , cdKeywords = singleton Keyword.Hunter
      }
