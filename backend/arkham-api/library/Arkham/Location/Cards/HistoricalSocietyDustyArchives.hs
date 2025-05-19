module Arkham.Location.Cards.HistoricalSocietyDustyArchives (historicalSocietyDustyArchives) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HistoricalSocietyDustyArchives = HistoricalSocietyDustyArchives LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

historicalSocietyDustyArchives :: LocationCard HistoricalSocietyDustyArchives
historicalSocietyDustyArchives = location HistoricalSocietyDustyArchives Cards.historicalSocietyDustyArchives 3 (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyDustyArchives where
  getModifiersFor (HistoricalSocietyDustyArchives attrs) = do
    modifySelect attrs InvestigatorWithHiddenCard [CannotInvestigateLocation attrs.id]

instance RunMessage HistoricalSocietyDustyArchives where
  runMessage msg (HistoricalSocietyDustyArchives attrs) =
    HistoricalSocietyDustyArchives <$> runMessage msg attrs
