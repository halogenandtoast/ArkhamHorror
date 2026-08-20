module Arkham.Treachery.CardDefs.TheDreamEaters.WhereTheGodsDwell where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

abandonedByTheGods :: CardDef
abandonedByTheGods =
  (treachery "06322" "Abandoned by the Gods" WhereTheGodsDwell 2)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril]
    }

myriadForms :: CardDef
myriadForms =
  (treachery "06318" "Myriad Forms" WhereTheGodsDwell 2)
    { cdCardTraits = singleton Power
    }

restlessJourneyFallacy :: CardDef
restlessJourneyFallacy =
  (treachery "06319" ("Restless Journey" <:> "Fallacy") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

restlessJourneyHardship :: CardDef
restlessJourneyHardship =
  (treachery "06320" ("Restless Journey" <:> "Hardship") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

restlessJourneyLies :: CardDef
restlessJourneyLies =
  (treachery "06321" ("Restless Journey" <:> "Lies") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Curse
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosEast :: CardDef
whisperingChaosEast =
  (treachery "06315" ("Whispering Chaos" <:> "East") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosNorth :: CardDef
whisperingChaosNorth =
  (treachery "06314" ("Whispering Chaos" <:> "North") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosSouth :: CardDef
whisperingChaosSouth =
  (treachery "06316" ("Whispering Chaos" <:> "South") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }

whisperingChaosWest :: CardDef
whisperingChaosWest =
  (treachery "06317" ("Whispering Chaos" <:> "West") WhereTheGodsDwell 1)
    { cdCardTraits = singleton Power
    , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
    }
