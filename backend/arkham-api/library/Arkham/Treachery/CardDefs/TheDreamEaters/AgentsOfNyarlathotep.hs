module Arkham.Treachery.CardDefs.TheDreamEaters.AgentsOfNyarlathotep where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

lawOfYgirothChaos :: CardDef
lawOfYgirothChaos =
  (treachery "06087" ("Law of 'Ygiroth" <:> "Chaos") AgentsOfNyarlathotep 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

lawOfYgirothDiscord :: CardDef
lawOfYgirothDiscord =
  (treachery "06088" ("Law of 'Ygiroth" <:> "Discord") AgentsOfNyarlathotep 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

lawOfYgirothPandemonium :: CardDef
lawOfYgirothPandemonium =
  (treachery "06089" ("Law of 'Ygiroth" <:> "Pandemonium") AgentsOfNyarlathotep 1)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }
