module Arkham.Treachery.Cards.NightmarishVapors (nightmarishVapors) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NightmarishVapors = NightmarishVapors TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightmarishVapors :: TreacheryCard NightmarishVapors
nightmarishVapors = treachery NightmarishVapors Cards.nightmarishVapors

instance RunMessage NightmarishVapors where
  runMessage msg t@(NightmarishVapors attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      remainingActions <- field InvestigatorRemainingActions iid
      cards <- getTekelili 2

      chooseOneM iid do
        when (remainingActions >= 2 || length cards < 2) do
          labeled "Lose 2 actions." $ loseActions iid attrs 2

        when (length cards >= 2 || remainingActions < 2) do
          labeled "Shuffle the top 2 cards of the Tekeli-li deck into your deck without looking at them."
            $ addTekelili iid cards

      pure t
    _ -> NightmarishVapors <$> liftRunMessage msg attrs
