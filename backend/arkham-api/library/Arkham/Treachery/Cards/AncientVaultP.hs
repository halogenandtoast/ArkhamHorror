module Arkham.Treachery.Cards.AncientVaultP (ancientVaultP) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Types (Field (..))
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AncientVaultP = AncientVaultP TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultP :: TreacheryCard AncientVaultP
ancientVaultP = treachery AncientVaultP Cards.ancientVaultP

instance HasAbilities AncientVaultP where
  getAbilities (AncientVaultP a) = case a.attached.location of
    Just lid ->
      [ restricted a 1 OnSameLocation
          $ actionAbilityWithCost
          $ CalculatedHandDiscardCost (LocationMaybeFieldCalculation lid LocationShroud) #any
      ]
    Nothing -> []

instance RunMessage AncientVaultP where
  runMessage msg t@(AncientVaultP attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Revelation cannot be canceled; attach to your location.
      withLocationOf iid (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- The discard cost is paid as the ability's cost; flip this card over.
      flipOver iid attrs
      pure t
    Flip iid _ (isTarget attrs -> True) -> do
      -- The back (11610b) is a story card that translates the glyph and adds itself
      -- to the victory display. A treachery has no UI slot a story can replace, so
      -- the runner focuses the story card and waits for the player to click it.
      readStory iid attrs Stories.ancientVaultP
      pure t
    _ -> AncientVaultP <$> liftRunMessage msg attrs
