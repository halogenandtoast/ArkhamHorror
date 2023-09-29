module Arkham.Event.Cards.GetOverHere2 (
  getOverHere2,
  GetOverHere2 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.SkillType

newtype GetOverHere2 = GetOverHere2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getOverHere2 :: EventCard GetOverHere2
getOverHere2 =
  event GetOverHere2 Cards.getOverHere2

instance RunMessage GetOverHere2 where
  runMessage msg e@(GetOverHere2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      let m = LocationWithId lid
      enemies <- selectList $ NonEliteEnemy <> EnemyAt (LocationMatchAny [m, ConnectedFrom m])
      push
        $ chooseOne
          iid
          [ targetLabel
            enemy
            [ EnemyEngageInvestigator enemy iid
            , FightEnemy iid enemy (toSource attrs) Nothing SkillCombat False
            ]
          | enemy <- enemies
          ]
      pure e
    _ -> GetOverHere2 <$> runMessage msg attrs
