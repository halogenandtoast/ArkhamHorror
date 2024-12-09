module Arkham.Event.Events.ExistentialRiddle1 (existentialRiddle1, ExistentialRiddle1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.SkillTest.Target
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Placement
import Arkham.Projection

newtype ExistentialRiddle1 = ExistentialRiddle1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

existentialRiddle1 :: EventCard ExistentialRiddle1
existentialRiddle1 = event ExistentialRiddle1 Cards.existentialRiddle1

instance HasModifiersFor ExistentialRiddle1 where
  getModifiersFor (ExistentialRiddle1 a) = case a.attachedTo of
    Just target -> modified_ a target [AddKeyword Aloof]
    _ -> pure mempty

instance RunMessage ExistentialRiddle1 where
  runMessage msg e@(ExistentialRiddle1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> NonEliteEnemy <> canParleyEnemy iid
      pure e
    HandleTargetChoice iid (is attrs -> True) (EnemyTarget eid) -> do
      handLength <- fieldMap InvestigatorHand length iid
      sid <- getRandom
      chooseOne
        iid
        [ SkillLabel sType [Msg.parley sid iid attrs eid sType (Fixed $ max 0 (8 - handLength))]
        | sType <- [#willpower, #intellect]
        ]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= traverse_ \case
        EnemyTarget eid -> pushAll [EnemyEvaded iid eid, PlaceEvent attrs.id $ AttachedToEnemy eid]
        _ -> pure ()
      pure e
    _ -> ExistentialRiddle1 <$> liftRunMessage msg attrs
