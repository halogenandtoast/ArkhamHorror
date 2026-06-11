module Arkham.Treachery.Cards.VanishingHistory (vanishingHistory) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Trait (Trait (Item))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VanishingHistory = VanishingHistory TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vanishingHistory :: TreacheryCard VanishingHistory
vanishingHistory = treachery VanishingHistory Cards.vanishingHistory

instance RunMessage VanishingHistory where
  runMessage msg t@(VanishingHistory attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      items <- select $ assetControlledBy iid <> AssetWithTrait Item <> AssetNonStory
      handCount <- length <$> iid.hand
      chooseOrRunOneM iid $ scenarioI18n do
        when (notNull items) do
          labeled' "vanishingHistory.discardItem" do
            chooseTargetM iid items \item -> toDiscardBy iid attrs item
        when (handCount > 0) do
          labeled' "vanishingHistory.discardCards" do
            repeated 3 $ chooseAndDiscardCard iid attrs
      pure t
    _ -> VanishingHistory <$> liftRunMessage msg attrs
