{- | The Masque of the Red Death's enemies.

The Red Death itself is set aside until act 1 advances and has no printed health
at all: it cannot be engaged and cannot be fought, only outrun. It is Aloof and
a Hunter, and at the start of every enemy phase it attacks each investigator,
[[Humanoid]] enemy and [[Victim]] story asset in its room -- the guests die with
you.

Prospero Prince's enemy side is the back of his asset
("Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets"), so he is declared
here with 'doubleSided' pointing back at it.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

-- | The back of the Prospero Prince asset. Only an enemy once that card flips.
prosperoPrinceDevoteeOfKassogtha :: CardDef
prosperoPrinceDevoteeOfKassogtha =
  unique
    $ doubleSided ":the-masque-of-the-red-death:023"
    $ ( enemy
          ":the-masque-of-the-red-death:023b"
          ("Prospero Prince" <:> "Devotee of Kassogtha")
          Set.TheMasqueOfTheRedDeath
          1
      )
      { cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdFight = fight 4
      , cdHealth = healthPerInvestigator 4
      , cdEvade = evade 4
      , cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Alert]
      , cdVictoryPoints = Just 1
      }

-- | No printed health: it is never defeated, only avoided.
theRedDeath :: CardDef
theRedDeath =
  (enemy ":the-masque-of-the-red-death:024" "The Red Death" Set.TheMasqueOfTheRedDeath 1)
    { cdCardTraits = setFromList [Avatar, Elite]
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    , cdUnique = True
    }

infectedGuest :: CardDef
infectedGuest =
  (enemy ":the-masque-of-the-red-death:025" "Infected Guest" Set.TheMasqueOfTheRedDeath 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdFight = fight 2
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

maddenedReveler :: CardDef
maddenedReveler =
  (enemy ":the-masque-of-the-red-death:028" "Maddened Reveler" Set.TheMasqueOfTheRedDeath 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

pestilenceCarrier :: CardDef
pestilenceCarrier =
  (enemy ":the-masque-of-the-red-death:031" "Pestilence Carrier" Set.TheMasqueOfTheRedDeath 2)
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }

redSpectre :: CardDef
redSpectre =
  (enemy ":the-masque-of-the-red-death:033" "Red Spectre" Set.TheMasqueOfTheRedDeath 2)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }
