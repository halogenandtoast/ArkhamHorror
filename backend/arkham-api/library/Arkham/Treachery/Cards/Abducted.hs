module Arkham.Treachery.Cards.Abducted (abducted) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ally))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Abducted = Abducted TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abducted :: TreacheryCard Abducted
abducted = treachery Abducted Cards.abducted

instance RunMessage Abducted where
  runMessage msg t@(Abducted attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      allies <- select $ AssetWithTrait Ally <> AssetAt (locationWithInvestigator iid)
      if null allies
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ AssetWithTrait Ally <> AssetAt (locationWithInvestigator iid)
      for_ allies exhaustThis
      chooseTargetM iid allies \ally -> dealAssetHorror ally attrs 5
      pure t
    _ -> Abducted <$> liftRunMessage msg attrs
