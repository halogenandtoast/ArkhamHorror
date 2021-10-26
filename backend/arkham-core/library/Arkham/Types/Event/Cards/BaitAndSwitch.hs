module Arkham.Types.Event.Cards.BaitAndSwitch where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards (baitAndSwitch)
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (EnemyEvaded)
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

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
    Successful (Action.Evade, EnemyTarget eid) iid _ target
      | isTarget attrs target -> do
        nonElite <- notMember eid <$> select EliteEnemy
        let msgs = EnemyEvaded iid eid : [ WillMoveEnemy eid msg | nonElite ]
        e <$ pushAll msgs
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target)
      | isTarget attrs target -> do
        lid <- getId @LocationId iid
        connectedLocationIds <- selectList ConnectedLocation
        e <$ unless
          (null connectedLocationIds)
          (withQueue_ \queue ->
            let
              enemyMoves = map (EnemyMove enemyId lid) connectedLocationIds
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
