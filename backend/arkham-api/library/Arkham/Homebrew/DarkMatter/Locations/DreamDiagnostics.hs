module Arkham.Homebrew.DarkMatter.Locations.DreamDiagnostics (dreamDiagnostics) where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  brainAttachedTo,
  brainsAttachedTo,
  printedIcons,
  scan,
  scanAction_,
 )
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DreamDiagnostics = DreamDiagnostics LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamDiagnostics :: LocationCard DreamDiagnostics
dreamDiagnostics = symbolLabel $ location DreamDiagnostics Cards.dreamDiagnostics 3 (Static 0)

{- | "[action] If Reality Simulator is in play: Scan. Search the scanning deck
for a card with both this location's icon and the icon on a [[Brain]] story
asset attached to this location. Draw it. If it is a location, put that card
into play on top of Reality Simulator."
-}
instance HasAbilities DreamDiagnostics where
  getAbilities (DreamDiagnostics a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> exists (brainAttachedTo a.id) <> exists (locationIs Cards.realitySimulator))
        scanAction_

instance RunMessage DreamDiagnostics where
  runMessage msg l@(DreamDiagnostics attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      brains <- brainsAttachedTo attrs.id
      symbol <- field LocationPrintedSymbol attrs.id
      chooseTargetM iid brains \aid -> do
        icons <- fieldMap AssetCard printedIcons aid
        -- a scanned location is placed on Reality Simulator by 'drawScannedCard'
        scan iid (attrs.ability 1) (symbol : icons)
      pure l
    _ -> DreamDiagnostics <$> liftRunMessage msg attrs
