module Arkham.Location.Cards.EasternAthenaeum (easternAthenaeum) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype EasternAthenaeum = EasternAthenaeum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easternAthenaeum :: LocationCard EasternAthenaeum
easternAthenaeum = location EasternAthenaeum Cards.easternAthenaeum 1 (Static 1)

instance HasAbilities EasternAthenaeum where
  getAbilities (EasternAthenaeum a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage EasternAthenaeum where
  runMessage msg l@(EasternAthenaeum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- The GroupClueCost above has already spent 1 clue per investigator.
      -- "You discover this glyph (rune_b)." Record "Plant" under rune_b; translated.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_b" :: Text, "Plant" :: Text)
      pure l
    _ -> EasternAthenaeum <$> liftRunMessage msg attrs
