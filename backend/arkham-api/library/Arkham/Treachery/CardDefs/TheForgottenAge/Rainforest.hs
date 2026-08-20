module Arkham.Treachery.CardDefs.TheForgottenAge.Rainforest where

import Arkham.Treachery.CardDefs.Import

overgrowth :: CardDef
overgrowth =
  (treachery "04076" "Overgrowth" Rainforest 2)
    { cdCardTraits = singleton Obstacle
    }

voiceOfTheJungle :: CardDef
voiceOfTheJungle =
  (treachery "04077" "Voice of the Jungle" Rainforest 2)
    { cdCardTraits = singleton Power
    }
