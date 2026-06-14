module Arkham.Location.Cards.LuminousArchivesArchiveOfMemory (luminousArchivesArchiveOfMemory) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Glyph))

newtype LuminousArchivesArchiveOfMemory = LuminousArchivesArchiveOfMemory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luminousArchivesArchiveOfMemory :: LocationCard LuminousArchivesArchiveOfMemory
luminousArchivesArchiveOfMemory = location LuminousArchivesArchiveOfMemory Cards.luminousArchivesArchiveOfMemory 2 (Static 3)

instance HasAbilities LuminousArchivesArchiveOfMemory where
  getAbilities (LuminousArchivesArchiveOfMemory a) =
    extendRevealed
      a
      [ restricted a 1 (thisExists a LocationWithAnyClues) $ forced $ PhaseEnds #when #investigation
      , restricted a 2 Here doubleActionAbility
      ]

instance RunMessage LuminousArchivesArchiveOfMemory where
  runMessage msg l@(LuminousArchivesArchiveOfMemory attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      glyphs <- select $ TreacheryAttachedToLocation (be attrs) <> TreacheryWithTrait Glyph
      for_ glyphs $ shuffleIntoDeck Deck.EncounterDeck
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- Search the encounter discard pile for a Glyph card and draw it.
      findEncounterCardIn iid attrs (CardWithTrait Glyph) [FromEncounterDiscard]
      pure l
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      drawCard iid card
      pure l
    _ -> LuminousArchivesArchiveOfMemory <$> liftRunMessage msg attrs
