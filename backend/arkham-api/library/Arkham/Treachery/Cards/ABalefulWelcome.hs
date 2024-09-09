module Arkham.Treachery.Cards.ABalefulWelcome (aBalefulWelcome, ABalefulWelcome (..)) where

import Arkham.Helpers.History
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ABalefulWelcome = ABalefulWelcome TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aBalefulWelcome :: TreacheryCard ABalefulWelcome
aBalefulWelcome = treachery ABalefulWelcome Cards.aBalefulWelcome

instance RunMessage ABalefulWelcome where
  runMessage msg t@(ABalefulWelcome attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      drawn <- getAllHistoryField PhaseHistory HistoryTreacheriesDrawn
      let secondCopy = elem attrs.cardCode drawn
      let choose action = eachInvestigator \i -> roundModifier attrs i (CannotTakeAction action)
      chooseNM iid (if secondCopy then 3 else 2) do
        labeled "Investigate" $ choose #investigate
        labeled "Fight" $ choose #fight
        labeled "Evade" $ choose #evade
        labeled "Move" $ choose #move
        labeled "Play" $ choose #play
      pure t
    _ -> ABalefulWelcome <$> liftRunMessage msg attrs
