module Arkham.Event.Cards.CloseCall2 (
  closeCall2,
  CloseCall2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Window
import Arkham.Window qualified as Window

newtype CloseCall2 = CloseCall2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeCall2 :: EventCard CloseCall2
closeCall2 = event CloseCall2 Cards.closeCall2

instance RunMessage CloseCall2 where
  runMessage msg e@(CloseCall2 attrs) = case msg of
    InvestigatorPlayEvent _iid eid _ [(windowType -> Window.EnemyEvaded _ enemyId)] _ | attrs `is` eid -> do
      push $ ShuffleBackIntoEncounterDeck (toTarget enemyId)
      pure e
    _ -> CloseCall2 <$> runMessage msg attrs
