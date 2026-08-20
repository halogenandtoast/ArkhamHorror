module Arkham.Enemy.CardDefs.ReturnToTheCircleUndone.ReturnToAtDeathsDoorstep where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

dmitriKonstantinovTakingTheLongView :: CardDef
dmitriKonstantinovTakingTheLongView =
  (enemy "54026" ("Dmitri Konstantinov" <:> "Taking the Long View") ReturnToAtDeathsDoorstep 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    }

senatorNathanielRhodesAdeptPolitician :: CardDef
senatorNathanielRhodesAdeptPolitician =
  (enemy "54025" ("Senator Nathanial Rhodes" <:> "Adept Politician") ReturnToAtDeathsDoorstep 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }
