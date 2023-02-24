module Arkham.Event.Cards.BuryThemDeep
  ( buryThemDeep
  , BuryThemDeep(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window
import Arkham.Window qualified as Window

newtype BuryThemDeep = BuryThemDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buryThemDeep :: EventCard BuryThemDeep
buryThemDeep = event BuryThemDeep Cards.buryThemDeep

instance RunMessage BuryThemDeep where
  runMessage msg e@(BuryThemDeep attrs) = case msg of
    InvestigatorPlayEvent _ eid _ [Window Timing.After (Window.EnemyDefeated _ enemyId)] _
      | eid == toId attrs
      -> do
        push $ AddToVictory (toTarget attrs)
        e <$ replaceMessage
          (Discard (toSource attrs) $ EnemyTarget enemyId)
          [AddToVictory (EnemyTarget enemyId)]
    _ -> BuryThemDeep <$> runMessage msg attrs
