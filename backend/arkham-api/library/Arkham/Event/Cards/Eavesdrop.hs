module Arkham.Event.Cards.Eavesdrop (
  eavesdrop,
  Eavesdrop (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Discover
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype Eavesdrop = Eavesdrop EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eavesdrop :: EventCard Eavesdrop
eavesdrop = event Eavesdrop Cards.eavesdrop

instance RunMessage Eavesdrop where
  runMessage msg e@(Eavesdrop attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectMap EnemyTarget
          $ UnengagedEnemy
          <> EnemyWithEvade
          <> EnemyAt
            (locationWithInvestigator iid)
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne
            player
            [ TargetLabel target [HandleTargetChoice iid (toSource attrs) target]
            | target <- targets
            ]
        ]
      pure e
    HandleTargetChoice iid source (EnemyTarget eid) | isSource attrs source -> do
      sid <- getRandom
      push $ beginSkillTest sid iid attrs attrs #intellect (EnemyMaybeFieldCalculation eid EnemyEvade)
      pure e
    PassedSkillTest iid _ _ target _ _ | isTarget attrs target -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toSource attrs) 2
      pure e
    _ -> Eavesdrop <$> runMessage msg attrs
