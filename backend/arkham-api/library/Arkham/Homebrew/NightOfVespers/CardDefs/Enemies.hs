module Arkham.Homebrew.NightOfVespers.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.NightOfVespers.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

declanODoyle :: CardDef
declanODoyle =
  (enemy ":night-of-vespers:031" ("Declan O'Doyle" <:> "Reanimated Racketeer") Set.TheEndOfSeasons 1)
    { cdCardTraits = setFromList [Humanoid, Criminal, Risen, Elite]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    }

skulkingHooligan :: CardDef
skulkingHooligan =
  (enemy ":night-of-vespers:036" "Skulking Hooligan" Set.TheEndOfSeasons 3)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdKeywords = singleton Keyword.Hunter
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 0
    }

syndicateFiringSquad :: CardDef
syndicateFiringSquad =
  (enemy ":night-of-vespers:339" "Syndicate Firing Squad" Set.TheFamily 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 1
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 0
    }

hiredTriggermen :: CardDef
hiredTriggermen =
  (enemy ":night-of-vespers:341" "Hired Triggermen" Set.TheFamily 2)
    { cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    }
