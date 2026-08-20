module Arkham.Act.Cards (allActCards) where

import Arkham.Act.CardDefEntries (allActCardDefs)
import Arkham.Homebrew.Defs qualified as Homebrew

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Prelude hiding (fold)

allActCards :: Map CardCode CardDef
allActCards =
  (Homebrew.actsMap <>) $ mapFromList $ map (toCardCode &&& id) allActCardDefs
