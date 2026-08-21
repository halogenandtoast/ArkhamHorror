module Arkham.Treachery.Cards.ChildrenOfBlood.Hunted.NowhereToHide (nowhereToHide) where

import Arkham.Discard
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Hunted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NowhereToHide = NowhereToHide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nowhereToHide :: TreacheryCard NowhereToHide
nowhereToHide = treachery NowhereToHide Cards.nowhereToHide

instance RunMessage NowhereToHide where
  runMessage msg t@(NowhereToHide attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      canDiscardAsset <- selectAny $ DiscardableAsset <> assetControlledBy iid
      canDiscardCards <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      when (canDiscardAsset || canDiscardCards) do
        chooseOneM iid $ withI18n do
          countVar 1 $ labeledValidate' canDiscardAsset "discardAssets" $ chooseAndDiscardAsset iid attrs
          countVar 2
            $ labeledValidate' canDiscardCards "discardCardsFromHand"
            $ discardFromHand iid attrs DiscardChoose 2
      pure t
    _ -> NowhereToHide <$> liftRunMessage msg attrs
