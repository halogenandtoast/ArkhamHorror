module Arkham.Event.Cards.Oops2 (
  oops2,
  Oops2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Oops2 = Oops2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

oops2 :: EventCard Oops2
oops2 = event Oops2 Cards.oops2

toEnemyId :: [Window] -> EnemyId
toEnemyId = \case
  [windowType -> Window.FailAttackEnemy _ enemy _] -> enemy
  _ -> error "expected one FailAttackEnemy window"

instance RunMessage Oops2 where
  runMessage msg e@(Oops2 attrs) = case msg of
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
      pushAll
        [ skillTestModifier (toSource attrs) iid DoesNotDamageOtherInvestigator
        , CancelFailedByModifierEffects
        , damageMsg
        ]
      pure e
    _ -> Oops2 <$> runMessage msg attrs
