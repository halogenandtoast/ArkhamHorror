module Arkham.Event.Cards.BaitAndSwitch where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards (baitAndSwitch)
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype BaitAndSwitch = BaitAndSwitch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch :: EventCard BaitAndSwitch
baitAndSwitch = event BaitAndSwitch Cards.baitAndSwitch

instance (EventRunner env) => RunMessage env BaitAndSwitch where
  runMessage msg e@(BaitAndSwitch attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ ChooseEvadeEnemy
        iid
        (EventSource eid)
        (Just $ toTarget attrs)
        SkillAgility
        AnyEnemy
        False
      , Discard (EventTarget eid)
      ]
    Successful (Action.Evade, EnemyTarget eid) iid _ target _
      | isTarget attrs target -> do
        nonElite <- notMember eid <$> select EliteEnemy
        let msgs = EnemyEvaded iid eid : [ WillMoveEnemy eid msg | nonElite ]
        e <$ pushAll msgs
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _)
      | isTarget attrs target -> do
        connectedLocationIds <- selectList ConnectedLocation
        e <$ unless
          (null connectedLocationIds)
          (withQueue_ \queue ->
            let
              enemyMoves = map (EnemyMove enemyId) connectedLocationIds
              (before, rest) = break
                (\case
                  AfterEvadeEnemy{} -> True
                  _ -> False
                )
                queue
            in case rest of
              (x : xs) -> before <> [x, chooseOne iid enemyMoves] <> xs
              _ -> error "evade missing"
          )
    _ -> BaitAndSwitch <$> runMessage msg attrs
