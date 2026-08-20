module Arkham.Enemy.CardDefs.TheScarletKeys.SanguineShadows where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

apportionedKa :: CardDef
apportionedKa =
  (enemy "09564" "Apportioned Ka" SanguineShadows 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Ritual, Elite]
    , cdKeywords = setFromList [Keyword.Concealed ApportionedKa (Static 4)]
    }

boundNightgaunt :: CardDef
boundNightgaunt =
  (enemy "09558" "Bound Nightgaunt" SanguineShadows 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

laChicaRojaTheGirlInTheCarmineCoat :: CardDef
laChicaRojaTheGirlInTheCarmineCoat =
  doubleSided "09557b"
    $ (enemy "09557" ("La Chica Roja" <:> "The Girl in the Carmine Coat") SanguineShadows 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
      , cdKeywords = setFromList [Keyword.Concealed LaChicaRoja (Static 5)]
      , cdVictoryPoints = Just 1
      , cdUnique = True
      }

theSanguineWatcherWithTheRubySpectacles :: CardDef
theSanguineWatcherWithTheRubySpectacles =
  (enemy "09563" ("The Sanguine Watcher" <:> "With the Ruby Spectacles") SanguineShadows 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }
