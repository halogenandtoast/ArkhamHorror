module Arkham.Event.Cards.ManoAMano1 (
  manoAMano1,
  ManoAMano1 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype ManoAMano1 = ManoAMano1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manoAMano1 :: EventCard ManoAMano1
manoAMano1 = event ManoAMano1 Cards.manoAMano1

instance RunMessage ManoAMano1 where
  runMessage msg e@(ManoAMano1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- select $ enemyEngagedWith iid
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne
            player
            [ targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 1]
            | enemy <- enemies
            ]
        ]
      pure e
    _ -> ManoAMano1 <$> runMessage msg attrs
