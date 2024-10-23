module Arkham.Treachery.Cards.RealmOfMadness (realmOfMadness, RealmOfMadness (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RealmOfMadness = RealmOfMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realmOfMadness :: TreacheryCard RealmOfMadness
realmOfMadness = treachery RealmOfMadness Cards.realmOfMadness

assetMatcher :: InvestigatorId -> AssetMatcher
assetMatcher iid = DiscardableAsset <> assetControlledBy iid

instance RunMessage RealmOfMadness where
  runMessage msg t@(RealmOfMadness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      hasAssets <- selectAny $ assetMatcher iid
      hasDiscardableCards <- fieldP InvestigatorHand (any (`cardMatch` DiscardableCard)) iid
      if horror > 0 && (hasAssets || hasDiscardableCards)
        then doStep horror msg
        else assignHorror iid attrs 2
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) | n > 0 -> do
      assets <- selectWithField AssetCost $ assetMatcher iid
      handDiscardableCards <- fieldMap InvestigatorHand (filterCards DiscardableCard) iid

      chooseOneM iid do
        for_ assets \(asset, cost) -> do
          targeting asset do
            toDiscardBy iid attrs asset
            doStep (n - cost) msg'
        targets handDiscardableCards \card -> do
          toDiscardBy iid attrs (toCardId card)
          doStep (n - card.printedCost) msg'
      pure t
    _ -> RealmOfMadness <$> liftRunMessage msg attrs
