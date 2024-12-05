module Arkham.Location.Cards.HistoricalSocietyRecordOffice_138 (
  historicalSocietyRecordOffice_138,
  HistoricalSocietyRecordOffice_138 (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg

newtype HistoricalSocietyRecordOffice_138 = HistoricalSocietyRecordOffice_138 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyRecordOffice_138
  :: LocationCard HistoricalSocietyRecordOffice_138
historicalSocietyRecordOffice_138 =
  location
    HistoricalSocietyRecordOffice_138
    Cards.historicalSocietyRecordOffice_138
    2
    (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyRecordOffice_138 where
  getModifiersFor (HistoricalSocietyRecordOffice_138 a) =
    whenRevealed a $ modifySelect a (enemyAt a) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities HistoricalSocietyRecordOffice_138 where
  getAbilities (HistoricalSocietyRecordOffice_138 a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #when (be a) AnyEnemy
      | a.unrevealed
      ]

instance RunMessage HistoricalSocietyRecordOffice_138 where
  runMessage msg l@(HistoricalSocietyRecordOffice_138 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Msg.RevealLocation Nothing attrs.id
      pure l
    _ -> HistoricalSocietyRecordOffice_138 <$> liftRunMessage msg attrs
