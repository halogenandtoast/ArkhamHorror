module Arkham.Treachery.Cards.LowOnSupplies (lowOnSupplies) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LowOnSupplies = LowOnSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lowOnSupplies :: TreacheryCard LowOnSupplies
lowOnSupplies = treachery LowOnSupplies Cards.lowOnSupplies

instance RunMessage LowOnSupplies where
  runMessage msg t@(LowOnSupplies attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      anyWithResources <- selectAny InvestigatorWithAnyResources
      hasAssets <- selectAny (HasMatchingAsset AnyAsset)
      investigators <- getInvestigators
      chooseOrRunOneM iid $ campaignI18n do
        when anyWithResources do
          labeled' "lowOnSupplies.resources" $ for_ investigators (loseResourcesOf attrs 2)
        labeled' "lowOnSupplies.damage" $ for_ investigators (assignDamageTo attrs 1)
        when hasAssets do
          labeled' "lowOnSupplies.discardAsset" $ for_ investigators (`chooseAndDiscardAsset` attrs)
      pure t
    _ -> LowOnSupplies <$> liftRunMessage msg attrs
