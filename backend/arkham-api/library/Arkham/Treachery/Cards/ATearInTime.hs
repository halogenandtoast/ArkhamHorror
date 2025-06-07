module Arkham.Treachery.Cards.ATearInTime (aTearInTime) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ATearInTime = ATearInTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInTime :: TreacheryCard ATearInTime
aTearInTime = treachery ATearInTime Cards.aTearInTime

instance RunMessage ATearInTime where
  runMessage msg t@(ATearInTime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasRemainingActions <- fieldP InvestigatorRemainingActions (> 0) iid
      chooseOrRunOneM iid $ withI18n do
        when hasRemainingActions $ countVar 1 $ labeled' "loseActions" $ loseActions iid attrs 1
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> ATearInTime <$> liftRunMessage msg attrs
