module Arkham.Treachery.Cards.AncientVaultP (ancientVaultP) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Key
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted.Log (record)
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
      -- Resolve the flipped side (11610b). The back story is not registered as a
      -- Story card, so we resolve its known mechanical effects directly: discover
      -- this glyph, record its translation, and add this card to the victory display.
      -- TODO: verify the exact glyph word printed on 11610b (placeholder "Weather").
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_p" :: Text, "Weather" :: Text)
      addToVictory iid attrs
      pure t
    _ -> AncientVaultP <$> liftRunMessage msg attrs
