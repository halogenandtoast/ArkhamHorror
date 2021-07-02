module Arkham.Types.Event.Cards.BaitAndSwitch where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards (baitAndSwitch)
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype BaitAndSwitch = BaitAndSwitch EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baitAndSwitch :: InvestigatorId -> EventId -> BaitAndSwitch
baitAndSwitch = event BaitAndSwitch Cards.baitAndSwitch

instance HasModifiersFor env BaitAndSwitch where
  getModifiersFor = noModifiersFor

instance HasActions env BaitAndSwitch where
  getActions i window (BaitAndSwitch attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env BaitAndSwitch where
  runMessage msg e@(BaitAndSwitch attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ ChooseEvadeEnemy iid (EventSource eid) SkillAgility False
      , Discard (EventTarget eid)
      ]
    PassedSkillTest iid _ (EventSource eid) SkillTestInitiatorTarget{} _ _
      | eid == eventId -> do
        lid <- getId iid
        connectedLocationIds <- map unConnectedLocationId <$> getSetList lid
        target <- fromMaybe (error "missing target") <$> getSkillTestTarget
        case target of
          EnemyTarget enemyId -> do
            unless (null connectedLocationIds) $ withQueue_ $ \queue ->
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

            pure e
          _ -> error "wrong target type"
    _ -> BaitAndSwitch <$> runMessage msg attrs
