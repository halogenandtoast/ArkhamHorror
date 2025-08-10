module Arkham.Treachery.Cards.Ants (ants) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Ants = Ants TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ants :: TreacheryCard Ants
ants = treachery Ants Cards.ants

-- Interpretation, since this card avoids the word must for hand discard and
-- targetting language for the in play we can choose to discard a hand card
-- even if our hand is empty

instance RunMessage Ants where
  runMessage msg t@(Ants attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      hasDiscardableAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      chooseOrRunOneM iid $ withI18n do
        labeled' "discardFromHand" do
          randomDiscard iid attrs
          doStep (n - 1) msg'
        when hasDiscardableAssets do
          labeled' "discardFromPlay" do
            chooseAndDiscardAsset iid attrs
            doStep (n - 1) msg'
      pure t
    _ -> Ants <$> liftRunMessage msg attrs
