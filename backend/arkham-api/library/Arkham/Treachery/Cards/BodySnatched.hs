module Arkham.Treachery.Cards.BodySnatched (bodySnatched) where

import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ally, Item))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BodySnatched = BodySnatched TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bodySnatched :: TreacheryCard BodySnatched
bodySnatched = treachery BodySnatched Cards.bodySnatched

instance RunMessage BodySnatched where
  runMessage msg t@(BodySnatched attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <-
        selectWithField AssetCard
          $ assetControlledBy iid
          <> hasAnyTrait [Ally, Item]
          <> AssetCanLeavePlayByNormalMeans
      if null assets
        then assignHorror iid attrs 2
        else chooseOneM iid $ for assets \(aid, card) -> targeting aid $ hollow iid card
      pure t
    _ -> BodySnatched <$> liftRunMessage msg attrs
