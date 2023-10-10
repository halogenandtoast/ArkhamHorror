module Arkham.Event.Cards.Oops (
  oops,
  Oops (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Oops = Oops EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

toEnemyId :: [Window] -> EnemyId
toEnemyId = \case
  [windowType -> Window.FailAttackEnemy _ enemy _] -> enemy
  _ -> error "expected one FailAttackEnemy window"

instance RunMessage Oops where
  runMessage msg e@(Oops attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (toEnemyId -> enemy) _ | eid == toId attrs -> do
      player <- getPlayer iid
      enemies <- filter (/= enemy) <$> selectList (enemyAtLocationWith iid)
      let
        damageMsg = case enemies of
          [] -> error "event should not have been playable"
          [x] -> InvestigatorDamageEnemy iid x (toSource enemy)
          xs ->
            chooseOne player
              $ [ targetLabel x [InvestigatorDamageEnemy iid x (toSource enemy)]
                | x <- xs
                ]
      pushAll [CancelFailedByModifierEffects, damageMsg]
      pure e
    _ -> Oops <$> runMessage msg attrs
