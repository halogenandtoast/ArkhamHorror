module Arkham.Treachery.Cards.ForbiddenSecrets (forbiddenSecrets) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token (Token (Clue))
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
        then do
          gainSurge attrs
          pure t
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed 3)
          pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasClues <- (> 0) <$> field InvestigatorClues iid
      mLoc <- field InvestigatorLocation iid
      if hasClues
        then chooseOneM iid do
          labeled "Place 1 clue on your location" do
            case mLoc of
              Nothing -> pure ()
              Just loc -> moveTokens (toSource attrs) (toSource iid) (toTarget loc) Clue 1
          labeled "Take 1 horror" $ assignHorror iid (toSource attrs) 1
        else assignHorror iid (toSource attrs) 1
      doStep (n - 1) msg'
      pure t
    _ -> ForbiddenSecrets <$> liftRunMessage msg attrs
