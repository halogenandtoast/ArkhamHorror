module Arkham.Treachery.Cards.VanishingHistory (vanishingHistory) where

import Arkham.Discard (HandDiscard (..))
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCardEdit)
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
      nonWeaknessCount <- selectCount $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseOrRunOneM iid $ scenarioI18n do
        when (notNull items) do
          labeled' "vanishingHistory.discardItem" do
            chooseTargetM iid items \item -> toDiscardBy iid attrs item
        when (nonWeaknessCount > 0) do
          labeled' "vanishingHistory.discardCards" do
            chooseAndDiscardCardEdit iid attrs \handDiscard ->
              handDiscard
                { discardAmount = min 3 nonWeaknessCount
                , discardFilter = NonWeakness
                }
      pure t
    _ -> VanishingHistory <$> liftRunMessage msg attrs
