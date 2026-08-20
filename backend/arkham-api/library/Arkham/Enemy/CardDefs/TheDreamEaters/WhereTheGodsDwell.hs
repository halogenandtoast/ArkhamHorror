module Arkham.Enemy.CardDefs.TheDreamEaters.WhereTheGodsDwell where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

dholeOfTheWastes :: CardDef
dholeOfTheWastes =
  (enemy "06312" "Dhole of the Wastes" WhereTheGodsDwell 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 6
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = singleton Monster
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

highPriestNotToBeDescribed :: CardDef
highPriestNotToBeDescribed =
  unique
    $ (enemy "06311" ("High Priest Not to Be Described" <:> "Agent of the Other Gods") WhereTheGodsDwell 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Monster, Cultist, Avatar, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

liarWithNoFace :: CardDef
liarWithNoFace =
  (enemy "06313" "Liar with No Face" WhereTheGodsDwell 3)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Cultist, Servitor]
    , cdKeywords = singleton Keyword.Hunter
    }

nyarlathotepGodOfAThousandForms :: CardDef
nyarlathotepGodOfAThousandForms =
  unique
    $ (enemy "06309" ("Nyarlathotep" <:> "God of a Thousand Forms") WhereTheGodsDwell 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 5
      , cdHealth = health 6
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepMessengerOfTheOuterGods :: CardDef
nyarlathotepMessengerOfTheOuterGods =
  unique
    $ (enemy "06308" ("Nyarlathotep" <:> "Messenger of the Outer Gods") WhereTheGodsDwell 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepStalkerAmongTheStars :: CardDef
nyarlathotepStalkerAmongTheStars =
  unique
    $ (enemy "06310" ("Nyarlathotep" <:> "Stalker Among the Stars") WhereTheGodsDwell 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 7
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Massive]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepTheCrawlingChaos :: CardDef
nyarlathotepTheCrawlingChaos =
  unique
    $ (enemy "06306" ("Nyarlathotep" <:> "The Crawling Chaos") WhereTheGodsDwell 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }

nyarlathotepTheFacelessWhisperer :: CardDef
nyarlathotepTheFacelessWhisperer =
  unique
    $ (enemy "06307" ("Nyarlathotep" <:> "The Faceless Whisperer") WhereTheGodsDwell 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdRevelation = IsRevelation
      }
