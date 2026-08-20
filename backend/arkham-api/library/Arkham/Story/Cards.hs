module Arkham.Story.Cards (allStoryCards) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Prelude
import Arkham.Story.CardDefEntries (allStoryCardDefs)

allStoryCards :: Map CardCode CardDef
allStoryCards =
  ((Homebrew.storiesMap <> Homebrew.playerStoriesMap) <>)
    $ mapFromList
    $ map (toCardCode &&& id) allStoryCardDefs
