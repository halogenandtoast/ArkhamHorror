module Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

horrifyingShade :: CardDef
horrifyingShade =
  (enemy "08584" "Horrifying Shade" FatalMirage 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Eidolon]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

memoryOfAHuntGoneAwry :: CardDef
memoryOfAHuntGoneAwry =
  doubleSided "08575b"
    $ (enemy "08575" "Memory of a Hunt Gone Awry" FatalMirage 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      }

memoryOfALostPatient :: CardDef
memoryOfALostPatient =
  doubleSided "08576b"
    $ (enemy "08576" "Memory of a Lost Patient" FatalMirage 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfAMissingFather :: CardDef
memoryOfAMissingFather =
  doubleSided "08577b"
    $ (enemy "08577" "Memory of a Missing Father" FatalMirage 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfARavagedCountry :: CardDef
memoryOfARavagedCountry =
  doubleSided "08578b"
    $ (enemy "08578" "Memory of a Ravaged Country" FatalMirage 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfARegretfulVoyage :: CardDef
memoryOfARegretfulVoyage =
  doubleSided "08579b"
    $ (enemy "08579" "Memory of a Regretful Voyage" FatalMirage 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      }

memoryOfATerribleDiscovery :: CardDef
memoryOfATerribleDiscovery =
  doubleSided "08581b"
    $ (enemy "08581" "Memory of a Terrible Discovery" FatalMirage 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnAlienTranslation :: CardDef
memoryOfAnAlienTranslation =
  doubleSided "08582b"
    $ (enemy "08582" "Memory of an Alien Transformation" FatalMirage 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnUnrequitedLove :: CardDef
memoryOfAnUnrequitedLove =
  doubleSided "08583b"
    $ (enemy "08583" "Memory of an Unrequited Love" FatalMirage 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      }

memoryOfAnUnspeakableEvil :: CardDef
memoryOfAnUnspeakableEvil =
  doubleSided "08580b"
    $ (enemy "08580" "Memory of an Unspeakable Evil" FatalMirage 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Monster, Eidolon, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      }
