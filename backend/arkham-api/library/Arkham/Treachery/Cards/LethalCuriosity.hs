module Arkham.Treachery.Cards.LethalCuriosity (lethalCuriosity) where

import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LethalCuriosity = LethalCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lethalCuriosity :: TreacheryCard LethalCuriosity
lethalCuriosity = treachery LethalCuriosity Cards.lethalCuriosity

instance RunMessage LethalCuriosity where
  runMessage msg t@(LethalCuriosity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      chooseOrRunOneM iid do
        labeled "Take 1 damage" $ assignDamage iid attrs 1
        when canPlaceClues
          $ labeled "Place 1 of your clues on your location"
          $ placeCluesOnLocation iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> LethalCuriosity <$> liftRunMessage msg attrs
