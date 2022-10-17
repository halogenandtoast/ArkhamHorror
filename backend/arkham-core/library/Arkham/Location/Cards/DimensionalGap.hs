module Arkham.Location.Cards.DimensionalGap
  ( dimensionalGap
  , DimensionalGap(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( dimensionalGap )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Timing qualified as Timing

newtype DimensionalGap = DimensionalGap LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalGap :: LocationCard DimensionalGap
dimensionalGap = location DimensionalGap Cards.dimensionalGap 3 (PerPlayer 1)

instance HasAbilities DimensionalGap where
  getAbilities (DimensionalGap attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage DimensionalGap where
  runMessage msg l@(DimensionalGap attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      l <$ push
        (DiscardEncounterUntilFirst source Nothing $ CardWithType EnemyType)
    RequestedEncounterCard source _ (Just ec) | isSource attrs source ->
      l <$ push (SpawnEnemyAt (EncounterCard ec) (toId attrs))
    _ -> DimensionalGap <$> runMessage msg attrs
