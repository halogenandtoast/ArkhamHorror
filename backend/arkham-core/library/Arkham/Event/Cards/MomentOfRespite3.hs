module Arkham.Event.Cards.MomentOfRespite3 (
  momentOfRespite3,
  MomentOfRespite3 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Message

newtype MomentOfRespite3 = MomentOfRespite3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentOfRespite3 :: EventCard MomentOfRespite3
momentOfRespite3 = event MomentOfRespite3 Cards.momentOfRespite3

instance RunMessage MomentOfRespite3 where
  runMessage msg e@(MomentOfRespite3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      mHealHorror <- getHealHorrorMessage attrs 3 iid
      pushAll $ maybeToList mHealHorror <> [drawing]
      pure e
    _ -> MomentOfRespite3 <$> runMessage msg attrs
