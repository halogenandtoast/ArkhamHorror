module Arkham.Treachery.Cards.BestLaidPlans (bestLaidPlans) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BestLaidPlans = BestLaidPlans TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bestLaidPlans :: TreacheryCard BestLaidPlans
bestLaidPlans = treachery BestLaidPlans Cards.bestLaidPlans

instance RunMessage BestLaidPlans where
  runMessage msg t@(BestLaidPlans attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- iid.remainingActions
      chooseOneM iid do
        when (n > 0) do
          withI18n $ countVar 2 $ labeled' "loseActions" $ loseActions iid attrs 2
        campaignI18n $ labeled' "bestLaidPlans.shuffle" $ doStep 1 msg
      pure t
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      getEncounterDeck >>= \case
        Deck [] -> pure ()
        Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      pure t
    _ -> BestLaidPlans <$> liftRunMessage msg attrs
