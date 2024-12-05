module Arkham.Location.Cards.HistoricalSocietyRecordOffice_129 (
  historicalSocietyRecordOffice_129,
  HistoricalSocietyRecordOffice_129 (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)

newtype HistoricalSocietyRecordOffice_129 = HistoricalSocietyRecordOffice_129 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyRecordOffice_129
  :: LocationCard HistoricalSocietyRecordOffice_129
historicalSocietyRecordOffice_129 =
  location
    HistoricalSocietyRecordOffice_129
    Cards.historicalSocietyRecordOffice_129
    2
    (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyRecordOffice_129 where
  getModifiersFor (HistoricalSocietyRecordOffice_129 a) =
    modifySelect a (enemyAt a) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities HistoricalSocietyRecordOffice_129 where
  getAbilities (HistoricalSocietyRecordOffice_129 a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #when (be a) AnyEnemy
      | not a.revealed
      ]

instance RunMessage HistoricalSocietyRecordOffice_129 where
  runMessage msg l@(HistoricalSocietyRecordOffice_129 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reveal attrs.id
      pure l
    _ -> HistoricalSocietyRecordOffice_129 <$> liftRunMessage msg attrs
