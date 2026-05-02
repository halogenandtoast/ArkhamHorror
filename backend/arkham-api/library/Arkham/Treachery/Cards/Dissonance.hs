module Arkham.Treachery.Cards.Dissonance (dissonance) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Dissonance = Dissonance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissonance :: TreacheryCard Dissonance
dissonance = treachery Dissonance Cards.dissonance

instance RunMessage Dissonance where
  runMessage msg t@(Dissonance attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      loseActions iid attrs 1
      chooseOneM iid $ withI18n do
        labeled' "assets" $ discardAll iid attrs $ #asset <> not_ #weakness
        labeled' "events" $ discardAll iid attrs $ #event <> not_ #weakness
        labeled' "skills" $ discardAll iid attrs $ #skill <> not_ #weakness
      pure t
    _ -> Dissonance <$> liftRunMessage msg attrs
