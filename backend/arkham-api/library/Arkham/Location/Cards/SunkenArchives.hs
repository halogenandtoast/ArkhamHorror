module Arkham.Location.Cards.SunkenArchives (sunkenArchives, SunkenArchives (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SunkenArchives = SunkenArchives LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenArchives :: LocationCard SunkenArchives
sunkenArchives = location SunkenArchives Cards.sunkenArchives 2 (PerPlayer 1)

instance HasAbilities SunkenArchives where
  getAbilities (SunkenArchives a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance HasModifiersFor SunkenArchives where
  getModifiersFor (SunkenArchives a) =
    modifySelect a Anyone [CannotDiscoverCluesAt $ be a <> FloodedLocation]

instance RunMessage SunkenArchives where
  runMessage msg l@(SunkenArchives attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs BlackKey
      increaseThisFloodLevelTo attrs FullyFlooded
      pure l
    _ -> SunkenArchives <$> liftRunMessage msg attrs
