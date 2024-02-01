module Arkham.Event.Cards.NothingLeftToLose3 (
  nothingLeftToLose3,
  NothingLeftToLose3 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype NothingLeftToLose3 = NothingLeftToLose3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

nothingLeftToLose3 :: EventCard NothingLeftToLose3
nothingLeftToLose3 = eventWith NothingLeftToLose3 Cards.nothingLeftToLose3 (afterPlayL .~ RemoveThisFromGame)

instance RunMessage NothingLeftToLose3 where
  runMessage msg e@(NothingLeftToLose3 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      resources <- field InvestigatorResources iid
      cards <- fieldMap InvestigatorHand length iid
      drawing <- drawCards iid attrs (5 - cards)
      pushAll
        $ [takeResources iid attrs (5 - resources) | resources < 5]
        <> [drawing | cards < 5]
      pure e
    _ -> NothingLeftToLose3 <$> runMessage msg attrs
