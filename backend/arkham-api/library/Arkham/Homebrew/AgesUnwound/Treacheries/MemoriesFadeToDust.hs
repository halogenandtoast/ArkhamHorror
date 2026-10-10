module Arkham.Homebrew.AgesUnwound.Treacheries.MemoriesFadeToDust (memoriesFadeToDust) where

import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype MemoriesFadeToDust = MemoriesFadeToDust TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoriesFadeToDust :: TreacheryCard MemoriesFadeToDust
memoriesFadeToDust = treachery MemoriesFadeToDust Cards.memoriesFadeToDust

{- | "Revelation - Test [intellect] (3). For each point you fail by, you must
either (choose one): Place one of your clues on your location. / Discard a random
card from your hand. / Take 1 horror."
-}
instance RunMessage MemoriesFadeToDust where
  runMessage msg t@(MemoriesFadeToDust attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      hasCards <- notNull <$> iid.hand
      chooseOneM iid $ withI18n do
        countVar 1
          $ labeledValidate canPlaceClues "placeCluesOnYourLocation"
          $ push
          $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1
        labeledValidate hasCards "discardRandomCard" $ randomDiscard iid attrs
        chooseTakeHorror iid attrs 1
      doNextStep msg
      pure t
    _ -> MemoriesFadeToDust <$> liftRunMessage msg attrs
