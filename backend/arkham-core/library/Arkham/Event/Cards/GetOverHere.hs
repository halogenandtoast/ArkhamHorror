module Arkham.Event.Cards.GetOverHere
  ( getOverHere
  , GetOverHere(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype GetOverHere = GetOverHere EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getOverHere :: EventCard GetOverHere
getOverHere =
  event GetOverHere Cards.getOverHere

instance RunMessage GetOverHere where
  runMessage msg e@(GetOverHere attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      m <- LocationWithId <$> getJustLocation
      enemies <- selectList $ NonEliteEnemy <> EnemyAt (m <> ConnectedFrom m)
      pushAll [Discard (toTarget attrs)]
    _ -> GetOverHere <$> runMessage msg attrs
