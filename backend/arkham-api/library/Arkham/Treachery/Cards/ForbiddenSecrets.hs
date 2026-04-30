module Arkham.Treachery.Cards.ForbiddenSecrets (forbiddenSecrets) where

import Arkham.Helpers.Location
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ForbiddenSecrets = ForbiddenSecrets TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenSecrets :: TreacheryCard ForbiddenSecrets
forbiddenSecrets = treachery ForbiddenSecrets Cards.forbiddenSecrets

instance RunMessage ForbiddenSecrets where
  runMessage msg t@(ForbiddenSecrets attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      clueCount <- field InvestigatorClues iid
      if clueCount == 0
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      if hasClues
        then chooseOneM iid $ withI18n do
          withLocationOf iid \_ -> do
            countVar 1 $ labeled' "placeCluesOnYourLocation" $ placeCluesOnLocation iid attrs 1
          countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
        else assignHorror iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> ForbiddenSecrets <$> liftRunMessage msg attrs
