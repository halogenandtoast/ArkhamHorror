module Arkham.Types.Event.Cards.HypnoticGaze
  ( hypnoticGaze
  , HypnoticGaze(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query

newtype HypnoticGaze = HypnoticGaze EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGaze :: EventCard HypnoticGaze
hypnoticGaze = event HypnoticGaze Cards.hypnoticGaze

instance HasActions env HypnoticGaze where
  getActions iid window (HypnoticGaze attrs) = getActions iid window attrs

instance HasModifiersFor env HypnoticGaze where
  getModifiersFor = noModifiersFor

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasCount HealthDamageCount env EnemyId => RunMessage env HypnoticGaze where
  runMessage msg e@(HypnoticGaze attrs) = case msg of
    InvestigatorPlayEvent iid eventId _ | eventId == toId attrs -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid : queue' = dropUntilAttack queue
        in (queue', eid)
      healthDamage' <- unHealthDamageCount <$> getCount enemyId
      e <$ push (EnemyDamage enemyId iid (toSource attrs) healthDamage')
    _ -> HypnoticGaze <$> runMessage msg attrs
