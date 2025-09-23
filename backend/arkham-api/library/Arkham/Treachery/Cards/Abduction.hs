module Arkham.Treachery.Cards.Abduction (abduction) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Abduction = Abduction TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abduction :: TreacheryCard Abduction
abduction = treachery Abduction Cards.abduction

instance RunMessage Abduction where
  runMessage msg t@(Abduction attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- selectTargets $ assetControlledBy iid <> #ally <> DiscardableAsset
      chooseOrRunOneM iid do
        labeled "Lose all resources" $ loseAllResources iid (toSource attrs)
        unless (null allies) do
          labeled "Discard an Ally asset you control" do
            chooseTargetM iid allies (toDiscardBy iid attrs)
      pure t
    _ -> Abduction <$> liftRunMessage msg attrs
