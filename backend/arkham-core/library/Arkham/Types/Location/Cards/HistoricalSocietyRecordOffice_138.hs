module Arkham.Types.Location.Cards.HistoricalSocietyRecordOffice_138
  ( historicalSocietyRecordOffice_138
  , HistoricalSocietyRecordOffice_138(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype HistoricalSocietyRecordOffice_138 = HistoricalSocietyRecordOffice_138 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyRecordOffice_138
  :: LocationCard HistoricalSocietyRecordOffice_138
historicalSocietyRecordOffice_138 = locationWithRevealedSideConnections
  HistoricalSocietyRecordOffice_138
  Cards.historicalSocietyRecordOffice_138
  2
  (PerPlayer 1)
  NoSymbol
  [Star]
  Equals
  [Star]

instance HasModifiersFor env HistoricalSocietyRecordOffice_138 where
  getModifiersFor _ (EnemyTarget eid) (HistoricalSocietyRecordOffice_138 attrs)
    = pure $ if eid `member` locationEnemies attrs
      then toModifiers attrs [EnemyFight 1, EnemyEvade 1]
      else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities HistoricalSocietyRecordOffice_138 where
  getAbilities (HistoricalSocietyRecordOffice_138 attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
        Timing.When
        (LocationWithId $ toId attrs)
        AnyEnemy
    | not (locationRevealed attrs)
    ]

instance LocationRunner env => RunMessage env HistoricalSocietyRecordOffice_138 where
  runMessage msg l@(HistoricalSocietyRecordOffice_138 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyRecordOffice_138 <$> runMessage msg attrs
