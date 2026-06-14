module Arkham.Location.Cards.ChamberOfRecordsArm (chamberOfRecordsArm) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Scenarios.TheGrandVault.Helpers

newtype ChamberOfRecordsArm = ChamberOfRecordsArm LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfRecordsArm :: LocationCard ChamberOfRecordsArm
chamberOfRecordsArm = location ChamberOfRecordsArm Cards.chamberOfRecordsArm 4 (Static 1)

instance HasAbilities ChamberOfRecordsArm where
  getAbilities (ChamberOfRecordsArm a) =
    extendRevealed a
      [ restricted a 1 Here actionAbility
      , mkAbility a 2 $ freeReaction $ DiscoveringLastClue #after You (be a)
      ]

instance RunMessage ChamberOfRecordsArm where
  runMessage msg l@(ChamberOfRecordsArm attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      activateLocation (attrs.ability 1) attrs.id
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- You discover this glyph (rune_l). Record "Arm" under rune_l; translated.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("Chamber of Records" :: Text, "Arm" :: Text)
      pure l
    _ -> ChamberOfRecordsArm <$> liftRunMessage msg attrs
