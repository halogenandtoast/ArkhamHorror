module Arkham.Event.Cards.Dodge2 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.SkillType
import Arkham.Target

newtype Metadata = Metadata { selectedEnemy :: Maybe EnemyId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Dodge2 = Dodge2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dodge2 :: EventCard Dodge2
dodge2 = event (Dodge2 . (`with` Metadata Nothing)) Cards.dodge2

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance RunMessage Dodge2 where
  runMessage msg e@(Dodge2 (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemyId <- fromQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack _ enemyId _ _ : _ -> enemyId
        _ -> error "unhandled"
      pushAll [CancelNext (toSource attrs) AttackMessage, beginSkillTest iid (toSource attrs) (InvestigatorTarget iid) SkillAgility 1]
      pure $ Dodge2 (attrs `with` Metadata (Just enemyId))
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _ -> do
      for_ (selectedEnemy meta) $ \enemyId -> 
        push $ EnemyDamage enemyId $ nonAttack (toSource attrs) 1
      pure e
    _ -> Dodge2 . (`with` meta) <$> runMessage msg attrs
