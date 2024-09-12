module Arkham.Treachery.Cards.Corrosion (corrosion, Corrosion (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Id
import Arkham.Investigator.Projection ()
import Arkham.Location.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Corrosion = Corrosion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosion :: TreacheryCard Corrosion
corrosion = treachery Corrosion Cards.corrosion

handMatcher :: CardMatcher
handMatcher = #item <> #asset <> NonWeakness

assetMatcher :: InvestigatorId -> AssetMatcher
assetMatcher iid = DiscardableAsset <> #item <> assetControlledBy iid

instance RunMessage Corrosion where
  runMessage msg t@(Corrosion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        shroud <- fromMaybe 0 <$> lid.shroud
        hasAssets <- selectAny $ assetMatcher iid
        hasHandAssets <- any (`cardMatch` handMatcher) <$> iid.hand
        if shroud > 0 && (hasAssets || hasHandAssets)
          then doStep shroud msg
          else gainSurge attrs
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) | n > 0 -> do
      assets <- selectWithField AssetCost $ assetMatcher iid
      handAssets <- iid.hand.filter handMatcher
      chooseOneM iid do
        for_ assets \(asset, cost) -> targeting asset do
          toDiscardBy iid attrs asset
          doStep (n - cost) msg'
        targets handAssets \card -> do
          toDiscardBy iid attrs card.id
          doStep (n - card.printedCost) msg'
      pure t
    _ -> Corrosion <$> liftRunMessage msg attrs
