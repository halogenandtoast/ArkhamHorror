module Arkham.Event.Cards.Waylay (
  waylay,
  Waylay (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Enemy
import Arkham.Matcher
import Arkham.SkillType

newtype Waylay = Waylay EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waylay :: EventCard Waylay
waylay = event Waylay Cards.waylay

instance RunMessage Waylay where
  runMessage msg e@(Waylay attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <- select $ NonEliteEnemy <> enemyAtLocationWith iid <> ExhaustedEnemy <> EnemyWithEvade
      player <- getPlayer iid
      sid <- getRandom
      pushAll
        [ chooseOne
            player
            [ targetLabel
              enemy
              [ beginSkillTest
                  sid
                  iid
                  (toSource attrs)
                  (EnemyTarget enemy)
                  SkillAgility
                  (EnemyMaybeFieldCalculation enemy EnemyEvade)
              ]
            | enemy <- enemies
            ]
        ]
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) (SkillTestInitiatorTarget (EnemyTarget eid)) _ _ ->
      do
        pushAllM $ defeatEnemy eid iid attrs
        pure e
    _ -> Waylay <$> runMessage msg attrs
