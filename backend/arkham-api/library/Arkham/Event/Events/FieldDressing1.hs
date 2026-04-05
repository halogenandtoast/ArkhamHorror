module Arkham.Event.Events.FieldDressing1 (fieldDressing1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype FieldDressing1 = FieldDressing1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldDressing1 :: EventCard FieldDressing1
fieldDressing1 = event FieldDressing1 Cards.fieldDressing1

instance RunMessage FieldDressing1 where
  runMessage msg e@(FieldDressing1 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 3 msg
      pure e
    DoStep n (PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      let source = toSource attrs
      healableInvestigators <- select $ HealableInvestigator source #damage (colocatedWith iid)
      healableAllies <- select $ HealableAsset source #damage $ AllyAsset <> assetControlledBy iid
      chooseOrRunOneM iid do
        targets healableInvestigators \iid' -> do
          healDamage iid' source 1
          doStep (n - 1) msg
        targets healableAllies \asset -> do
          healDamage asset source 1
          doStep (n - 1) msg
      pure e
    _ -> FieldDressing1 <$> liftRunMessage msg attrs
