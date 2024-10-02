module Arkham.Event.Events.Dodge2 (dodge2, Dodge2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Id

newtype Metadata = Metadata {selectedEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Dodge2 = Dodge2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dodge2 :: EventCard Dodge2
dodge2 = event (Dodge2 . (`with` Metadata Nothing)) Cards.dodge2

instance RunMessage Dodge2 where
  runMessage msg e@(Dodge2 (attrs `With` meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let currentAttack = getAttackDetails attrs.windows
      cancelAttack attrs currentAttack
      sid <- getRandom
      beginSkillTest sid iid attrs iid #agility $ Fixed 1
      pure $ Dodge2 (attrs `with` Metadata (Just currentAttack.enemy))
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      for_ (selectedEnemy meta) $ nonAttackEnemyDamage (toSource attrs) 1
      pure e
    _ -> Dodge2 . (`with` meta) <$> liftRunMessage msg attrs
