module Arkham.Location.Cards.DimensionalGap
  ( dimensionalGap
  , DimensionalGap(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (dimensionalGap)
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message hiding (RevealLocation)
import Arkham.Timing qualified as Timing

newtype DimensionalGap = DimensionalGap LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalGap :: LocationCard DimensionalGap
dimensionalGap = locationWith
  DimensionalGap
  Cards.dimensionalGap
  3
  (PerPlayer 1)
  NoSymbol
  []
  ((revealedSymbolL .~ T)
  . (revealedConnectedMatchersL .~ map LocationWithSymbol [Square, Moon])
  )

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

instance LocationRunner env => RunMessage env DimensionalGap where
  runMessage msg l@(DimensionalGap attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      l <$ push (DiscardEncounterUntilFirst source $ CardWithType EnemyType)
    RequestedEncounterCard source (Just ec) | isSource attrs source ->
      l <$ push (SpawnEnemyAt (EncounterCard ec) (toId attrs))
    _ -> DimensionalGap <$> runMessage msg attrs
