module Arkham.Act.Cards.TheGuardedRuins (
  TheGuardedRuins (..),
  theGuardedRuins,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheGuardedRuins = TheGuardedRuins ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGuardedRuins :: ActCard TheGuardedRuins
theGuardedRuins =
  act
    (3, A)
    TheGuardedRuins
    Cards.theGuardedRuins
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Ruins of Eztli"))

instance HasModifiersFor TheGuardedRuins where
  getModifiersFor (EnemyTarget eid) (TheGuardedRuins a) = do
    isEztliGuardian <- eid <=~> EnemyWithTitle "Eztli Guardian"
    pure $
      if isEztliGuardian
        then toModifiers a [EnemyFight 1, EnemyEvade 1]
        else []
  getModifiersFor (CardIdTarget cardId) (TheGuardedRuins a) = do
    card <- getCard cardId
    pure $
      toModifiers
        a
        [ AddKeyword Keyword.Surge
        | card `isCard` Treacheries.arrowsFromTheTrees
        ]
  getModifiersFor _ _ = pure []

instance RunMessage TheGuardedRuins where
  runMessage msg a@(TheGuardedRuins attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 2
      pure a
    _ -> TheGuardedRuins <$> runMessage msg attrs
