module Arkham.Location.Cards.HistoricalSocietyRecordOffice_129 (
  historicalSocietyRecordOffice_129,
  HistoricalSocietyRecordOffice_129 (..),
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

newtype HistoricalSocietyRecordOffice_129 = HistoricalSocietyRecordOffice_129 LocationAttrs
  deriving anyclass (IsLocation)
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
  getModifiersFor (EnemyTarget eid) (HistoricalSocietyRecordOffice_129 attrs) =
    pure $
      if eid `member` locationEnemies attrs
        then toModifiers attrs [EnemyFight 1, EnemyEvade 1]
        else []
  getModifiersFor _ _ = pure []

instance HasAbilities HistoricalSocietyRecordOffice_129 where
  getAbilities (HistoricalSocietyRecordOffice_129 attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1 $
        ForcedAbility $
          EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
      | not (locationRevealed attrs)
      ]

instance RunMessage HistoricalSocietyRecordOffice_129 where
  runMessage msg l@(HistoricalSocietyRecordOffice_129 attrs) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          l <$ push (Msg.RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyRecordOffice_129 <$> runMessage msg attrs
