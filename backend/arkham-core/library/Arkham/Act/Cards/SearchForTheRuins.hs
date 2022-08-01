module Arkham.Act.Cards.SearchForTheRuins
  ( SearchForTheRuins(..)
  , searchForTheRuins
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Target

newtype SearchForTheRuins = SearchForTheRuins ActAttrs
  deriving anyclass (IsAct, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheRuins :: ActCard SearchForTheRuins
searchForTheRuins = act
  (3, A)
  SearchForTheRuins
  Cards.searchForTheRuins
  (Just $ GroupClueCost (PerPlayer 3) (LocationWithTitle "Ruins of Eztli"))

instance HasModifiersFor SearchForTheRuins where
  getModifiersFor _ (EnemyTarget eid) (SearchForTheRuins a) = do
    isEztliGuardian <- eid <=~> EnemyWithTitle "Eztli Guardian"
    pure $ if isEztliGuardian
      then toModifiers a [CannotAttack, CannotBeAttacked]
      else []
  getModifiersFor _ (CardTarget card) (SearchForTheRuins a) = do
    let
      isArrowsFromTheTrees =
        cardMatch card (CardWithTitle "Arrows from the Trees")
    pure $ toModifiers a [ IgnoreRevelation | isArrowsFromTheTrees ]
  getModifiersFor _ _ _ = pure []

instance RunMessage SearchForTheRuins where
  runMessage msg a@(SearchForTheRuins attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    _ -> SearchForTheRuins <$> runMessage msg attrs
