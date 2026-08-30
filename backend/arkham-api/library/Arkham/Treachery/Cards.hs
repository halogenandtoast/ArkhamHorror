module Arkham.Treachery.Cards (
  allTreacheryCards,
  allPlayerTreacheryCards,
  allEncounterTreacheryCards,
) where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType (CardType (TreacheryType))
import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Prelude
import Arkham.Treachery.CardDefEntries (allTreacheryCardDefs)

allTreacheryCards :: Map CardCode CardDef
allTreacheryCards = allPlayerTreacheryCards <> allEncounterTreacheryCards

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
    $ filter ((== TreacheryType) . cdCardType) allTreacheryCardDefs
