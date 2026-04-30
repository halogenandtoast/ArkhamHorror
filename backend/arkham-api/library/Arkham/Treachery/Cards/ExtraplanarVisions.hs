module Arkham.Treachery.Cards.ExtraplanarVisions (extraplanarVisions) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ExtraplanarVisions = ExtraplanarVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraplanarVisions :: TreacheryCard ExtraplanarVisions
extraplanarVisions = treachery ExtraplanarVisions Cards.extraplanarVisions

instance RunMessage ExtraplanarVisions where
  runMessage msg t@(ExtraplanarVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseRevelationSkillTest sid iid attrs [#willpower, #intellect]
        $ InvestigatorHandLengthCalculation iid
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      hand <- field InvestigatorHand iid
      unless (null hand) $ randomDiscard iid attrs
      pure t
    _ -> ExtraplanarVisions <$> liftRunMessage msg attrs
