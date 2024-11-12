module Arkham.Treachery.Cards.TreacherousDepths (treacherousDepths, TreacherousDepths (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Location.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TreacherousDepths = TreacherousDepths TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousDepths :: TreacheryCard TreacherousDepths
treacherousDepths = treachery TreacherousDepths Cards.treacherousDepths

instance RunMessage TreacherousDepths where
  runMessage msg t@(TreacherousDepths attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        withLocationOf iid \lid -> do
          whenM (lid <=~> CanHaveFloodLevelIncreased) do
            labeled "Increase the flood level of your current location" $ increaseThisFloodLevel lid
          whenM (selectAny $ DiscardableAsset <> assetControlledBy iid) do
            labeled
              "Discard assets from your play area with a total resource cost of at least X, where X is your location's shroud value."
              do
                shroud <- lift $ fromMaybe 0 <$> lid.shroud
                doStep shroud msg
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) | n > 0 -> do
      assets <- selectWithField AssetCost $ DiscardableAsset <> assetControlledBy iid
      chooseOneM iid do
        for_ assets \(asset, cost) -> targeting asset do
          toDiscardBy iid attrs asset
          doStep (n - cost) msg'
      pure t
    _ -> TreacherousDepths <$> liftRunMessage msg attrs
