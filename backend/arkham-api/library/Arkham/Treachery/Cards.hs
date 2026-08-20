module Arkham.Treachery.Cards (
  allTreacheryCards,
  allPlayerTreacheryCards,
  allEncounterTreacheryCards,
) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Prelude
import Arkham.Treachery.CardDefEntries (allTreacheryCardDefs)

allTreacheryCards :: Map CardCode CardDef
allTreacheryCards = allPlayerTreacheryCards <> allEncounterTreacheryCards

{- | A treachery belongs to a player deck exactly when it carries a card
subtype (weakness or basic weakness); the rest are encounter cards.
-}
allPlayerTreacheryCards :: Map CardCode CardDef
allPlayerTreacheryCards =
  (Homebrew.playerTreacheriesMap <>)
    $ mapFromList
    $ concatMap toCardCodePairs
    $ filter (isJust . cdCardSubType) allTreacheryCardDefs

allEncounterTreacheryCards :: Map CardCode CardDef
allEncounterTreacheryCards =
  (Homebrew.treacheriesMap <>)
    $ mapFromList
    $ concatMap toCardCodePairs
    $ filter (isNothing . cdCardSubType) allTreacheryCardDefs
