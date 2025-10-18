module Arkham.Location.Cards.HistoricalSocietyDustyArchives (historicalSocietyDustyArchives) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HistoricalSocietyDustyArchives = HistoricalSocietyDustyArchives LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyDustyArchives :: LocationCard HistoricalSocietyDustyArchives
historicalSocietyDustyArchives = location HistoricalSocietyDustyArchives Cards.historicalSocietyDustyArchives 3 (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyDustyArchives where
  getModifiersFor (HistoricalSocietyDustyArchives attrs) = do
    modifySelect attrs InvestigatorWithHiddenCard [CannotInvestigateLocation attrs.id]

instance HasAbilities HistoricalSocietyDustyArchives where
  getAbilities (HistoricalSocietyDustyArchives a) =
    extendUnrevealed1 a $ mkAbility a 1 $ forced $ EnemySpawns #when (be a) AnyEnemy

instance RunMessage HistoricalSocietyDustyArchives where
  runMessage msg l@(HistoricalSocietyDustyArchives attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reveal attrs
      pure l
    _ -> HistoricalSocietyDustyArchives <$> liftRunMessage msg attrs
