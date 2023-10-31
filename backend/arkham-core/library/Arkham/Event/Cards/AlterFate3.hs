module Arkham.Event.Cards.AlterFate3 (
  alterFate3,
  AlterFate3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype AlterFate3 = AlterFate3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alterFate3 :: EventCard AlterFate3
alterFate3 = event AlterFate3 Cards.alterFate3

instance RunMessage AlterFate3 where
  runMessage msg e@(AlterFate3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      treacheries <- selectList $ NotTreachery (TreacheryOnEnemy EliteEnemy) <> TreacheryIsNonWeakness
      player <- getPlayer iid
      pushAll
        [ chooseOne player
            $ targetLabels treacheries
            $ only
            . toDiscardBy iid attrs
        ]
      pure e
    _ -> AlterFate3 <$> runMessage msg attrs
