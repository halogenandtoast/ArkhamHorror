module Arkham.Location.Cards.CorruptedVault (corruptedVault) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log (record)

newtype CorruptedVault = CorruptedVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corruptedVault :: LocationCard CorruptedVault
corruptedVault = location CorruptedVault Cards.corruptedVault 3 (Static 1)

instance HasAbilities CorruptedVault where
  getAbilities (CorruptedVault a) =
    extendRevealed1 a $ restricted a 1 Here doubleActionAbility

instance RunMessage CorruptedVault where
  runMessage msg l@(CorruptedVault attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- You discover this glyph (rune_q). Record "Hand" under rune_q; translated.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_q" :: Text, "Hand" :: Text)
      pure l
    _ -> CorruptedVault <$> liftRunMessage msg attrs
