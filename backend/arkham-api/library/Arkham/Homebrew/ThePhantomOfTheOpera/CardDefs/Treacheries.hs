{- | The three Always Watching cards are one treachery printed three ways; the
encounter deck holds one of each.
-}
module Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries where

import Arkham.Homebrew.ThePhantomOfTheOpera.Sets qualified as Set
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

alwaysWatchingBait :: CardDef
alwaysWatchingBait =
  (treachery ":the-phantom-of-the-opera:034" "Always Watching (Bait)" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = singleton Scheme
    }

alwaysWatchingFeint :: CardDef
alwaysWatchingFeint =
  (treachery ":the-phantom-of-the-opera:035" "Always Watching (Feint)" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = singleton Scheme
    }

alwaysWatchingGoad :: CardDef
alwaysWatchingGoad =
  (treachery ":the-phantom-of-the-opera:036" "Always Watching (Goad)" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = singleton Scheme
    }

angelOfMusic :: CardDef
angelOfMusic =
  (treachery ":the-phantom-of-the-opera:037" "Angel of Music" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Scheme, Terror]
    }

echoingThreats :: CardDef
echoingThreats =
  (treachery ":the-phantom-of-the-opera:038" "Echoing Threats" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Scheme, Terror]
    }

fixation :: CardDef
fixation =
  (treachery ":the-phantom-of-the-opera:039" "Fixation" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Scheme, Trap]
    , cdKeywords = singleton Keyword.Peril
    }

insidiousTrick :: CardDef
insidiousTrick =
  (treachery ":the-phantom-of-the-opera:040" "Insidious Trick" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = singleton Scheme
    }

shroudedFaces :: CardDef
shroudedFaces =
  (treachery ":the-phantom-of-the-opera:041" "Shrouded Faces" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Scheme, Power]
    , cdKeywords = singleton Keyword.Peril
    }

signsOfMadness :: CardDef
signsOfMadness =
  (treachery ":the-phantom-of-the-opera:042" "Signs of Madness" Set.ThePhantomOfTheOpera 3)
    { cdCardTraits = singleton Mystery
    }

thePhantomStrikes :: CardDef
thePhantomStrikes =
  (treachery ":the-phantom-of-the-opera:044" "The Phantom Strikes!" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Attack, Hazard]
    }
