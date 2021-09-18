module Arkham.Types.Event.Cards.BaitAndSwitch where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards (baitAndSwitch)
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.SkillTest
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
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> e <$ pushAll
      [ ChooseEvadeEnemy iid (EventSource eid) SkillAgility False
      , Discard (EventTarget eid)
      ]
    PassedSkillTest _ _ (EventSource eid) SkillTestInitiatorTarget{} _ _
      | eid == eventId -> do
        target <- fromMaybe (error "missing target") <$> getSkillTestTarget
        case target of
          EnemyTarget enemyId -> e <$ push (WillMoveEnemy enemyId msg)
          _ -> error "wrong target type"
    WillMoveEnemy enemyId (PassedSkillTest iid _ (EventSource eid) SkillTestInitiatorTarget{} _ _)
      | eid == eventId
      -> do
        lid <- getId iid
        connectedLocationIds <- map unConnectedLocationId <$> getSetList lid
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
