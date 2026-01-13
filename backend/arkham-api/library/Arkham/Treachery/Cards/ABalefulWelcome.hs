module Arkham.Treachery.Cards.ABalefulWelcome (aBalefulWelcome) where

import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.I18n
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ABalefulWelcome = ABalefulWelcome TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aBalefulWelcome :: TreacheryCard ABalefulWelcome
aBalefulWelcome = treachery ABalefulWelcome Cards.aBalefulWelcome

instance RunMessage ABalefulWelcome where
  runMessage msg t@(ABalefulWelcome attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> withI18n $ do
      isFirst <- isFirstCopyThisPhase attrs
      chooseNM iid (if isFirst then 2 else 3) do
        labeled' "investigate" do
          eachInvestigator \iid' -> roundModifier attrs iid' (CannotPerformAction #investigate)
        labeled' "fight" do
          eachInvestigator \iid' -> roundModifier attrs iid' (CannotPerformAction #fight)
        labeled' "evade" do
          eachInvestigator \iid' -> roundModifier attrs iid' (CannotPerformAction #evade)
        labeled' "move" do
          eachInvestigator \iid' -> roundModifier attrs iid' (CannotPerformAction #move)
        labeled' "play" do
          eachInvestigator \iid' -> roundModifier attrs iid' (CannotPerformAction #play)
      pure t
    _ -> ABalefulWelcome <$> liftRunMessage msg attrs
