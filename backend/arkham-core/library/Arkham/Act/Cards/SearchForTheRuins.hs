module Arkham.Act.Cards.SearchForTheRuins (
  SearchForTheRuins (..),
  searchForTheRuins,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Treachery.Cards qualified as Treacheries

newtype SearchForTheRuins = SearchForTheRuins ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForTheRuins :: ActCard SearchForTheRuins
searchForTheRuins =
  act
    (3, A)
    SearchForTheRuins
    Cards.searchForTheRuins
    (Just $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Ruins of Eztli"))

instance HasModifiersFor SearchForTheRuins where
  getModifiersFor (EnemyTarget eid) (SearchForTheRuins a) = do
    isEztliGuardian <- eid <=~> EnemyWithTitle "Eztli Guardian"
    pure $
      toModifiers a $
        guard isEztliGuardian
          *> [CannotAttack, CannotBeAttacked]
  getModifiersFor (TreacheryTarget tid) (SearchForTheRuins a) = do
    isArrowsFromTheTrees <- tid <=~> treacheryIs Treacheries.arrowsFromTheTrees
    pure $ toModifiers a [IgnoreRevelation | isArrowsFromTheTrees]
  getModifiersFor _ _ = pure []

instance RunMessage SearchForTheRuins where
  runMessage msg a@(SearchForTheRuins attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    _ -> SearchForTheRuins <$> runMessage msg attrs
