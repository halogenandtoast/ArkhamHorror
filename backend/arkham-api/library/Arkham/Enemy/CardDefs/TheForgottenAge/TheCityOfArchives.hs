module Arkham.Enemy.CardDefs.TheForgottenAge.TheCityOfArchives where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

keeperOfTheGreatLibrary :: CardDef
keeperOfTheGreatLibrary =
  (enemy "04257" "Keeper of the Great Library" TheCityOfArchives 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof]
    }

scholarFromYith :: CardDef
scholarFromYith =
  (enemy "04259" "Scholar from Yith" TheCityOfArchives 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Yithian]
    }

scientistOfYith :: CardDef
scientistOfYith =
  (enemy "04258" "Scientist of Yith" TheCityOfArchives 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Yithian]
    , cdKeywords = singleton Keyword.Aloof
    }
