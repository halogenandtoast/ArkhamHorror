module Arkham.Event.Cards.Eavesdrop
  ( eavesdrop
  , Eavesdrop(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype Eavesdrop = Eavesdrop EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eavesdrop :: EventCard Eavesdrop
eavesdrop = event Eavesdrop Cards.eavesdrop

instance RunMessage Eavesdrop where
  runMessage msg e@(Eavesdrop attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectListMap EnemyTarget $ UnengagedEnemy <> EnemyWithEvade <> EnemyAt
          (locationWithInvestigator iid)
      pushAll
        [ chooseOrRunOne
          iid
          [ TargetLabel target [HandleTargetChoice iid (toSource attrs) target]
          | target <- targets
          ]
        , Discard (toTarget attrs)
        ]
      pure e
    HandleTargetChoice iid source (EnemyTarget eid) | isSource attrs source ->
      do
        n <- fromJustNote "Enemy must have evade" <$> field EnemyEvade eid
        push $ BeginSkillTest
          iid
          (toSource attrs)
          (toTarget attrs)
          Nothing
          SkillIntellect
          n
        pure e
    PassedSkillTest iid _ _ target _ _ | isTarget attrs target -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 2 Nothing
      pure e
    _ -> Eavesdrop <$> runMessage msg attrs
