module Arkham.Homebrew.DarkMatter.Locations.Communicator (communicator) where

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
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Communicator = Communicator LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

communicator :: LocationCard Communicator
communicator = symbolLabel $ location Communicator Cards.communicator 2 (Static 0)

{- | "[action]: Scan. Search the scanning deck for a card with both this
location's icon and the icon on a [[Brain]] story asset attached to this
location. Draw it."
-}
instance HasAbilities Communicator where
  getAbilities (Communicator a) =
    extendRevealed1 a $ restricted a 1 (Here <> exists (brainAttachedTo a.id)) scanAction_

instance RunMessage Communicator where
  runMessage msg l@(Communicator attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      brains <- brainsAttachedTo attrs.id
      symbol <- field LocationPrintedSymbol attrs.id
      chooseTargetM iid brains \aid -> do
        icons <- fieldMap AssetCard printedIcons aid
        scan iid (attrs.ability 1) (symbol : icons)
      pure l
    _ -> Communicator <$> liftRunMessage msg attrs
