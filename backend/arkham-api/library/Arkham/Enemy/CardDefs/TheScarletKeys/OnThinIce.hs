module Arkham.Enemy.CardDefs.TheScarletKeys.OnThinIce where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

ravenousGrizzly :: CardDef
ravenousGrizzly =
  (enemy "09631" "Ravenous Grizzly" OnThinIce 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = singleton Creature
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

thorneTheOneWithTheRedCravat :: CardDef
thorneTheOneWithTheRedCravat =
  (enemy "09625" ("Thorne" <:> "The One With the Red Cravat") OnThinIce 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Hunter
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09625b"
    }

voidChimeraEarsplitter :: CardDef
voidChimeraEarsplitter =
  (enemy "09628" ("Void Chimera" <:> "Earsplitter") OnThinIce 1)
    { cdSanityDamage = sanityDamage 3
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraFellbeak :: CardDef
voidChimeraFellbeak =
  (enemy "09627" ("Void Chimera" <:> "Fellbeak") OnThinIce 1)
    { cdHealthDamage = healthDamage 3
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithMostInvestigators $ LocationWithTrait Wilderness), Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraFellhound :: CardDef
voidChimeraFellhound =
  (enemy "09630" ("Void Chimera" <:> "Fellhound") OnThinIce 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraGorefeaster :: CardDef
voidChimeraGorefeaster =
  (enemy "09629" ("Void Chimera" <:> "Gorefeaster") OnThinIce 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraTrueForm :: CardDef
voidChimeraTrueForm =
  (enemy "09626" ("Void Chimera" <:> "True Form") OnThinIce 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Concealed VoidChimeraTrueForm (Static 4), Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
