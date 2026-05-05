module Arkham.Event.Cards.Bounty (
  bounty,
  Bounty (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Bounty = Bounty EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bounty :: EventCard Bounty
bounty = event Bounty Cards.bounty

instance RunMessage Bounty where
  runMessage msg e@(Bounty attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ DoStep 1 msg
      pure e
    DoStep 1 (InvestigatorPlayEvent iid eid _ _ _) | eid == toId attrs -> do
      cultists <- select $ EnemyWithTrait Cultist <> EnemyAt (locationWithInvestigator iid)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [targetLabel cultist [EnemyDamage cultist $ nonAttackEnemyDamage iid 2] | cultist <- cultists]
      pure e
    _ -> Bounty <$> runMessage msg attrs
