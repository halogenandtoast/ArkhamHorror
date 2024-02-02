module Arkham.Location.Cards.HistoricalSocietyRecordOffice_138 (
  historicalSocietyRecordOffice_138,
  HistoricalSocietyRecordOffice_138 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message qualified as Msg
import Arkham.Timing qualified as Timing

newtype HistoricalSocietyRecordOffice_138 = HistoricalSocietyRecordOffice_138 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

historicalSocietyRecordOffice_138
  :: LocationCard HistoricalSocietyRecordOffice_138
historicalSocietyRecordOffice_138 =
  location
    HistoricalSocietyRecordOffice_138
    Cards.historicalSocietyRecordOffice_138
    2
    (PerPlayer 1)

instance HasModifiersFor HistoricalSocietyRecordOffice_138 where
  getModifiersFor (EnemyTarget eid) (HistoricalSocietyRecordOffice_138 attrs) = do
    atLocation <- enemyAtLocation eid attrs
    pure $ toModifiers attrs $ guard atLocation *> [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities HistoricalSocietyRecordOffice_138 where
  getAbilities (HistoricalSocietyRecordOffice_138 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
        $ ForcedAbility
        $ EnemySpawns
          Timing.When
          (LocationWithId $ toId attrs)
          AnyEnemy
      | not (locationRevealed attrs)
      ]

instance RunMessage HistoricalSocietyRecordOffice_138 where
  runMessage msg l@(HistoricalSocietyRecordOffice_138 attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          l <$ push (Msg.RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyRecordOffice_138 <$> runMessage msg attrs
