module Arkham.Treachery.Cards.TwinSuns (twinSuns) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TwinSuns = TwinSuns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twinSuns :: TreacheryCard TwinSuns
twinSuns = treachery TwinSuns Cards.twinSuns

instance RunMessage TwinSuns where
  runMessage msg t@(TwinSuns attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      agenda <- selectJust AnyAgenda
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "removeAgendaDoom" $ removeDoom attrs agenda 1
        countVar 1 $ labeled' "takeHorrorForEachPointOfFailure" $ assignHorror iid attrs n
      pure t
    _ -> TwinSuns <$> liftRunMessage msg attrs
