module Arkham.Location.Cards.FleshyPathsWesternBurrows (fleshyPathsWesternBurrows) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log (record)

newtype FleshyPathsWesternBurrows = FleshyPathsWesternBurrows LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshyPathsWesternBurrows :: LocationCard FleshyPathsWesternBurrows
fleshyPathsWesternBurrows = location FleshyPathsWesternBurrows Cards.fleshyPathsWesternBurrows 2 (Static 1)

instance HasAbilities FleshyPathsWesternBurrows where
  getAbilities (FleshyPathsWesternBurrows a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage FleshyPathsWesternBurrows where
  runMessage msg l@(FleshyPathsWesternBurrows attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("Fleshy Paths" :: Text, "Time" :: Text)
      pure l
    _ -> FleshyPathsWesternBurrows <$> liftRunMessage msg attrs
