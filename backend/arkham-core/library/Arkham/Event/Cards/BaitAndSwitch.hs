module Arkham.Event.Cards.BaitAndSwitch where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards ( baitAndSwitch )
import Arkham.Event.Runner
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype BaitAndSwitch = BaitAndSwitch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch :: EventCard BaitAndSwitch
baitAndSwitch = event BaitAndSwitch Cards.baitAndSwitch

instance RunMessage BaitAndSwitch where
  runMessage msg e@(BaitAndSwitch attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> e <$ pushAll
      [ ChooseEvadeEnemy
        iid
        (toSource attrs)
        (Just $ toTarget attrs)
        SkillAgility
        AnyEnemy
        False
      , Discard (EventTarget eid)
      ]
    Successful (Action.Evade, EnemyTarget eid) iid _ target _
      | isTarget attrs target -> do
        nonElite <- member eid <$> select NonEliteEnemy
        pushAll $ EnemyEvaded iid eid : [ WillMoveEnemy eid msg | nonElite ]
        pure e
    WillMoveEnemy enemyId (Successful (Action.Evade, _) iid _ target _)
      | isTarget attrs target -> do
        choices <- selectList ConnectedLocation
        let
          enemyMoveChoices = chooseOne
            iid
            [ targetLabel choice [EnemyMove enemyId choice]
            | choice <- choices
            ]
        insertAfterMatching
          [enemyMoveChoices]
          \case
            AfterEvadeEnemy{} -> True
            _ -> False
        pure e
    _ -> BaitAndSwitch <$> runMessage msg attrs
