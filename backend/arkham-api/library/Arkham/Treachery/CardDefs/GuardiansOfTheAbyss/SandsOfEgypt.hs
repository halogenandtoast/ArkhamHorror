module Arkham.Treachery.CardDefs.GuardiansOfTheAbyss.SandsOfEgypt where

import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.Import

abyssalReach :: CardDef
abyssalReach =
  (treachery "83054" "Abyssal Reach" SandsOfEgypt 3)
    { cdCardTraits = setFromList [Trait.Curse, Trait.Abyss]
    }

darkSacrifice :: CardDef
darkSacrifice =
  (treachery "83052" "Dark Sacrifice" SandsOfEgypt 2)
    { cdCardTraits = setFromList [Trait.Curse, Trait.Abyss]
    }

eclipse :: CardDef
eclipse =
  (treachery "83047" "Eclipse" SandsOfEgypt 3)
    { cdCardTraits = singleton Trait.Power
    }

sandstorm :: CardDef
sandstorm =
  (treachery "83048" "Sandstorm" SandsOfEgypt 3)
    { cdCardTraits = singleton Hazard
    }

slumber :: CardDef
slumber =
  (treachery "83051" "Slumber" SandsOfEgypt 2)
    { cdCardTraits = setFromList [Trait.Curse, Trait.Abyss]
    }

swarmOfLocusts :: CardDef
swarmOfLocusts =
  (treachery "83050" "Swarm of Locusts" SandsOfEgypt 3)
    { cdCardTraits = singleton Trait.Power
    }

terrorUnderThePyramids :: CardDef
terrorUnderThePyramids =
  (treachery "83049" "Terror Under the Pyramids" SandsOfEgypt 3)
    { cdCardTraits = singleton Scheme
    }

theBlackWind :: CardDef
theBlackWind =
  (treachery "83053" "The Black Wind" SandsOfEgypt 2)
    { cdCardTraits = singleton Trait.Power
    , cdKeywords = singleton Keyword.Peril
    , cdVictoryPoints = Just 1
    }
