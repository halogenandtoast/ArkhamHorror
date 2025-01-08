module Arkham.Act.Cards.TheGuardedRuins (TheGuardedRuins (..), theGuardedRuins) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheGuardedRuins = TheGuardedRuins ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theGuardedRuins :: ActCard TheGuardedRuins
theGuardedRuins =
  act
    (3, A)
    TheGuardedRuins
    Cards.theGuardedRuins
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Ruins of Eztli"))

instance HasModifiersFor TheGuardedRuins where
  getModifiersFor (TheGuardedRuins a) = do
    guardian <- modifySelect a (EnemyWithTitle "Eztli Guardian") [EnemyFight 1, EnemyEvade 1]
    validCards <- findAllCards (`isCard` Treacheries.arrowsFromTheTrees)
    cards <- modifyEach a validCards [AddKeyword Keyword.Surge]
    pure $ guardian <> cards

instance RunMessage TheGuardedRuins where
  runMessage msg a@(TheGuardedRuins attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 2
      pure a
    _ -> TheGuardedRuins <$> runMessage msg attrs
