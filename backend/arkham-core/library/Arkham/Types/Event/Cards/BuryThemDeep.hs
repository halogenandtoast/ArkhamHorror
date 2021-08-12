module Arkham.Types.Event.Cards.BuryThemDeep
  ( buryThemDeep
  , BuryThemDeep(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window
import qualified Arkham.Types.Window as W

newtype BuryThemDeep = BuryThemDeep EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buryThemDeep :: EventCard BuryThemDeep
buryThemDeep = event BuryThemDeep Cards.buryThemDeep

instance HasActions BuryThemDeep
instance HasModifiersFor env BuryThemDeep

instance RunMessage env BuryThemDeep where
  runMessage msg e@(BuryThemDeep attrs) = case msg of
    InvestigatorPlayEvent _ eid _ [Window Timing.After (W.EnemyDefeated _ enemyId)]
      | eid == toId attrs
      -> do
        push $ AddToVictory (toTarget attrs)
        e <$ replaceMessage
          (Discard $ EnemyTarget enemyId)
          [AddToVictory (EnemyTarget enemyId)]
    _ -> BuryThemDeep <$> runMessage msg attrs
