module Arkham.Enemy.CardDefs.TheDrownedCity.CourtOfTheAncients where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

colossalTyrant :: CardDef
colossalTyrant =
  unique
    $ (enemy "11635" ("Colossal Tyrant" <:> "Trapped in the Tower") CourtOfTheAncients 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Massive]
      , cdVictoryPoints = Just 1
      }

courtKeeperObserverOfDreams :: CardDef
courtKeeperObserverOfDreams =
  (enemy "11630" ("Court Keeper" <:> "Observer of Dreams") CourtOfTheAncients 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Keeper, Glyph, Elite]
    , cdVictoryPoints = Just 1
    }

courtKeeperWeaverOfNightmares :: CardDef
courtKeeperWeaverOfNightmares =
  (enemy "11631" ("Court Keeper" <:> "Weaver of Nightmares") CourtOfTheAncients 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Keeper, Glyph, Elite]
    , cdVictoryPoints = Just 1
    }

wingedKeeper :: CardDef
wingedKeeper =
  (enemy "11637" "Winged Keeper" CourtOfTheAncients 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Keeper]
    , cdKeywords = setFromList [Keyword.Alert]
    }
