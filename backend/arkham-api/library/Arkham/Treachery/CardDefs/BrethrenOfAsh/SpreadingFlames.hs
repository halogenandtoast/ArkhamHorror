module Arkham.Treachery.CardDefs.BrethrenOfAsh.SpreadingFlames where

import Arkham.Treachery.CardDefs.Import

forbiddenSecrets :: CardDef
forbiddenSecrets =
  (treachery "12126" "Forbidden Secrets" EldritchLore 2)
    { cdCardTraits = singleton Pact
    }

mutated1 :: CardDef
mutated1 =
  (treachery "12131" "Mutated!" MadScience 2)
    { cdCardTraits = singleton Mutation
    }

unspeakableTruths :: CardDef
unspeakableTruths =
  (treachery "12125" "Unspeakable Truths" EldritchLore 2)
    { cdCardTraits = singleton Terror
    }
