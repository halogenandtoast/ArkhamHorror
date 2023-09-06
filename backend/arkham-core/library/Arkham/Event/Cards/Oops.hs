module Arkham.Event.Cards.Oops (
  oops,
  Oops (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Oops = Oops EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

instance RunMessage Oops where
  runMessage msg e@(Oops attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [Window Timing.After (Window.FailAttackEnemy _ targetId _) _] _
      | eid == toId attrs -> do
          enemies <- filter (/= targetId) <$> selectList (EnemyAt $ locationWithInvestigator iid)
          let
            damageMsg = case enemies of
              [] -> error "event should not have been playable"
              [x] -> InvestigatorDamageEnemy iid x (EnemySource targetId)
              xs ->
                chooseOne iid $
                  [ targetLabel x [InvestigatorDamageEnemy iid x (EnemySource targetId)]
                  | x <- xs
                  ]
          pushAll [CancelFailedByModifierEffects, damageMsg]
          pure e
    _ -> Oops <$> runMessage msg attrs
