module Arkham.Event.Cards.Waylay
  ( waylay
  , Waylay(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype Waylay = Waylay EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waylay :: EventCard Waylay
waylay = event Waylay Cards.waylay

instance RunMessage Waylay where
  runMessage msg e@(Waylay attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemies <-
        selectWithField EnemyEvade
        $ NonEliteEnemy
        <> EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)
        <> ExhaustedEnemy
      let
        enemiesWithEvade =
          flip mapMaybe enemies $ \(enemy, mEvade) -> (enemy, ) <$> mEvade
      pushAll
        [ chooseOne
          iid
          [ targetLabel
              enemy
              [ beginSkillTest
                  iid
                  (toSource attrs)
                  (EnemyTarget enemy)
                  SkillAgility
                  evadeValue
              ]
          | (enemy, evadeValue) <- enemiesWithEvade
          ]
        ]
      pure e
    PassedSkillTest iid _ (isSource attrs -> True) (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      -> e <$ push (DefeatEnemy eid iid $ toSource attrs)
    _ -> Waylay <$> runMessage msg attrs
