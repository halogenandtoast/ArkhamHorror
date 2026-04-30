module Arkham.Treachery.Cards.ExtraplanarVisions (extraplanarVisions) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype ExtraplanarVisions = ExtraplanarVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraplanarVisions :: TreacheryCard ExtraplanarVisions
extraplanarVisions = treachery ExtraplanarVisions Cards.extraplanarVisions

instance RunMessage ExtraplanarVisions where
  runMessage msg t@(ExtraplanarVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      handCount <- fieldMap InvestigatorHand length iid
      sid <- getRandom
      chooseOneM iid do
        labeled "Test Intellect (X)" do
          beginSkillTest sid iid (toSource attrs) iid #willpower (Fixed handCount)
        labeled "Test Willpower (X)" do
          beginSkillTest sid iid (toSource attrs) iid #intellect (Fixed handCount)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      hand <- field InvestigatorHand iid
      unless (null hand) do
        randomDiscard iid attrs
      pure t
    _ -> ExtraplanarVisions <$> liftRunMessage msg attrs
