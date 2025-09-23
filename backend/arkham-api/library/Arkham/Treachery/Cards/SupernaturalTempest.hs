module Arkham.Treachery.Cards.SupernaturalTempest (supernaturalTempest) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SupernaturalTempest = SupernaturalTempest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

supernaturalTempest :: TreacheryCard SupernaturalTempest
supernaturalTempest = treachery SupernaturalTempest Cards.supernaturalTempest

instance RunMessage SupernaturalTempest where
  runMessage msg t@(SupernaturalTempest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 5)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      assets <- selectWithField AssetCost $ DiscardableAsset <> assetControlledBy iid
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
    _ -> SupernaturalTempest <$> liftRunMessage msg attrs
