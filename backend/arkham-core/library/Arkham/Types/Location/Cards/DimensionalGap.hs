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
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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

forcedAbility :: LocationAttrs -> Ability
forcedAbility a = mkAbility (toSource a) 1 LegacyForcedAbility

instance ActionRunner env => HasAbilities env DimensionalGap where
  getAbilities iid (Window Timing.After (RevealLocation who _)) (DimensionalGap attrs)
    | iid == who
    = pure [locationAbility (forcedAbility attrs)]
  getAbilities iid window (DimensionalGap attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env DimensionalGap where
  runMessage msg l@(DimensionalGap attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      l <$ push (DiscardEncounterUntilFirst source $ CardWithType EnemyType)
    RequestedEncounterCard source (Just ec) | isSource attrs source ->
      l <$ push (SpawnEnemyAt (EncounterCard ec) (toId attrs))
    _ -> DimensionalGap <$> runMessage msg attrs
