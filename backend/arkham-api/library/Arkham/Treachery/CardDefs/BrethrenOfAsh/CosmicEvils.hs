module Arkham.Treachery.CardDefs.BrethrenOfAsh.CosmicEvils where

import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.CardDefs.Import

cosmicEvils :: CardDef
cosmicEvils =
  (treachery "12124" "Cosmic Evils" CosmicEvils 3)
    { cdCardTraits = setFromList [Omen]
    , cdKeywords = setFromList [Keyword.Peril]
    }
