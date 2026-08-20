module Arkham.Enemy.CardDefs.TheScarletKeys.RedCoterie where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

alikiZoniUperetriaSpeaksInDeath :: CardDef
alikiZoniUperetriaSpeaksInDeath =
  (enemy "09761" ("Aliki Zoni Uperetria" <:> "Speaks in Death") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Aloof]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

amaranthScarletScorn :: CardDef
amaranthScarletScorn =
  (enemy "09759" ("Amaranth" <:> "Scarlet Scorn") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

desiderioDelgadoAlvarezRedInHisLedger :: CardDef
desiderioDelgadoAlvarezRedInHisLedger =
  (enemy "09758" ("Desiderio Delgado Alvarez" <:> "Red in His Ledger") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithEnemy (EnemyWithTrait Coterie <> not_ (EnemyIs "09758")))]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

laChicaRojaHotOnYourTrail :: CardDef
laChicaRojaHotOnYourTrail =
  (enemy "09753" ("La Chica Roja" <:> "Hot on Your Trail") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Aloof]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theBeastInACowlOfCrimsonLeavingATrailOfDestruction :: CardDef
theBeastInACowlOfCrimsonLeavingATrailOfDestruction =
  (enemy "09755" ("The Beast in a Cowl of Crimson" <:> "Leaving a Trail of Destruction") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theClaretKnightHoldsYouInContempt :: CardDef
theClaretKnightHoldsYouInContempt =
  (enemy "09756" ("The Claret Knight" <:> "Holds You in Contempt") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theRedGlovedManPurposeUnknown :: CardDef
theRedGlovedManPurposeUnknown =
  (enemy "09752" ("The Red-Gloved Man" <:> "Purpose Unknown") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theSanguineWatcherHeSeesWhatIsNotThere :: CardDef
theSanguineWatcherHeSeesWhatIsNotThere =
  (enemy "09754" ("The Sanguine Watcher" <:> "He Sees What Is Not There") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

thorneOpenToNegotiation :: CardDef
thorneOpenToNegotiation =
  (enemy "09757" ("Thorne" <:> "Open to Negotiation") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

tzuSanNiangAWhisperInYourEar :: CardDef
tzuSanNiangAWhisperInYourEar =
  (enemy "09760" ("Tzu San Niang" <:> "A Whisper in Your Ear") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }
