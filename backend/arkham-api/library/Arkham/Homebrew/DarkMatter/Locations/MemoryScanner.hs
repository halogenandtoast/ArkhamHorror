module Arkham.Homebrew.DarkMatter.Locations.MemoryScanner (memoryScanner) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Card.CardDef (toCardType)
import Arkham.Card.CardType
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  brainsAttachedTo,
  drawScannedCard,
  printedIcons,
  scanAction_,
  scanWith,
 )
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype MemoryScanner = MemoryScanner LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryScanner :: LocationCard MemoryScanner
memoryScanner = location MemoryScanner Cards.memoryScanner 3 (Static 0)

{- | "[action] If Reality Simulator is in play: Scan. Search the scanning deck
for a card with both this location's icon and the icon on a [[Brain]] story
asset attached to this location. Draw it. If it is a location, put that card
into play on top of Reality Simulator."
-}
instance HasAbilities MemoryScanner where
  getAbilities (MemoryScanner a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (locationIs Cards.realitySimulator)) scanAction_

instance RunMessage MemoryScanner where
  runMessage msg l@(MemoryScanner attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      brains <- brainsAttachedTo attrs.id
      symbol <- field LocationPrintedSymbol attrs.id
      chooseTargetM iid brains \aid -> do
        icons <- fieldMap AssetCard printedIcons aid
        scanWith iid (symbol : icons) \card ->
          case toCardType card of
            LocationType ->
              selectOne (locationIs Cards.realitySimulator) >>= \case
                Just simulator -> push $ ReplaceLocation simulator card Swap
                Nothing -> drawScannedCard iid (attrs.ability 1) card
            _ -> drawScannedCard iid (attrs.ability 1) card
      pure l
    _ -> MemoryScanner <$> liftRunMessage msg attrs
