module Arkham.Location.Cards.ChamberOfRecordsEarth (chamberOfRecordsEarth) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Scenarios.TheGrandVault.Helpers

newtype ChamberOfRecordsEarth = ChamberOfRecordsEarth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRecordsEarth :: LocationCard ChamberOfRecordsEarth
chamberOfRecordsEarth = location ChamberOfRecordsEarth Cards.chamberOfRecordsEarth 4 (Static 1)

instance HasAbilities ChamberOfRecordsEarth where
  getAbilities (ChamberOfRecordsEarth a) =
    extendRevealed a
      [ restricted a 1 Here actionAbility
      , mkAbility a 2 $ freeReaction $ DiscoveringLastClue #after You (be a)
      ]

instance RunMessage ChamberOfRecordsEarth where
  runMessage msg l@(ChamberOfRecordsEarth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      activateLocation (attrs.ability 1) attrs.id
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- You discover this glyph (rune_m). Record "Earth" under rune_m; translated.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_m" :: Text, "Earth" :: Text)
      pure l
    _ -> ChamberOfRecordsEarth <$> liftRunMessage msg attrs
