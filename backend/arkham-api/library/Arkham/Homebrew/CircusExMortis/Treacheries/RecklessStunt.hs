module Arkham.Homebrew.CircusExMortis.Treacheries.RecklessStunt (recklessStunt) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Performer))
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RecklessStunt = RecklessStunt TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recklessStunt :: TreacheryCard RecklessStunt
recklessStunt = treachery RecklessStunt Cards.recklessStunt

instance RunMessage RecklessStunt where
  runMessage msg t@(RecklessStunt attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      hasPerformer <- selectAny $ enemyAtLocationWith iid <> EnemyWithTrait Performer
      revelationSkillTest sid iid attrs #agility (Fixed $ if hasPerformer then 4 else 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        countVar 3 $ labeled' "loseResources" $ loseResources iid attrs 3
      pure t
    _ -> RecklessStunt <$> liftRunMessage msg attrs
