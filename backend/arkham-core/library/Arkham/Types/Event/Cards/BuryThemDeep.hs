module Arkham.Types.Event.Cards.BuryThemDeep
  ( buryThemDeep
  , BuryThemDeep(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window
import Arkham.Types.Window qualified as Window

newtype BuryThemDeep = BuryThemDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buryThemDeep :: EventCard BuryThemDeep
buryThemDeep = event BuryThemDeep Cards.buryThemDeep

instance RunMessage env BuryThemDeep where
  runMessage msg e@(BuryThemDeep attrs) = case msg of
    InvestigatorPlayEvent _ eid _ [Window Timing.After (Window.EnemyDefeated _ enemyId)]
      | eid == toId attrs
      -> do
        push $ AddToVictory (toTarget attrs)
        e <$ replaceMessage
          (Discard $ EnemyTarget enemyId)
          [AddToVictory (EnemyTarget enemyId)]
    _ -> BuryThemDeep <$> runMessage msg attrs
