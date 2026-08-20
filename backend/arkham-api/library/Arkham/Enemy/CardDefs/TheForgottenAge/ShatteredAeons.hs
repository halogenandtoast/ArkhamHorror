module Arkham.Enemy.CardDefs.TheForgottenAge.ShatteredAeons where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

alejandroVela :: CardDef
alejandroVela =
  unique
    $ doubleSided "04326b"
    $ (enemy "04326" ("Alejandro Vela" <:> "Or, Is He?") ShatteredAeons 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 6
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      }

formlessSpawn :: CardDef
formlessSpawn =
  (enemy "04337" ("Formless Spawn" <:> "From the Abyss") ShatteredAeons 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 10
    , cdCardTraits = setFromList [Monster, Abomination, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 2
    }

ichtacaScionOfYig :: CardDef
ichtacaScionOfYig =
  unique
    $ doubleSided "04325b"
    $ (enemy "04325" ("Ichtaca" <:> "Scion of Yig") ShatteredAeons 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Monster, Serpent, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

temporalDevourer :: CardDef
temporalDevourer =
  (enemy "04338" "Temporal Devourer" ShatteredAeons 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = singleton Keyword.Hunter
    }
