module Arkham.Location.Cards.HistoricalSocietyBoilerRoom (historicalSocietyBoilerRoom) where

import Arkham.Ability
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HistoricalSocietyBoilerRoom = HistoricalSocietyBoilerRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyBoilerRoom :: LocationCard HistoricalSocietyBoilerRoom
historicalSocietyBoilerRoom = location HistoricalSocietyBoilerRoom Cards.historicalSocietyBoilerRoom 1 (PerPlayer 1)

instance HasAbilities HistoricalSocietyBoilerRoom where
  getAbilities (HistoricalSocietyBoilerRoom a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) AnyValue

instance RunMessage HistoricalSocietyBoilerRoom where
  runMessage msg l@(HistoricalSocietyBoilerRoom attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      repeated n $ drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> HistoricalSocietyBoilerRoom <$> liftRunMessage msg attrs
