module Arkham.Types.Location.Cards.DimensionalGap
  ( dimensionalGap
  , DimensionalGap(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (dimensionalGap)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

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
  . (revealedConnectedSymbolsL .~ setFromList [Square, Moon])
  )

instance HasAbilities env DimensionalGap where
  getAbilities iid window (DimensionalGap attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1
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
