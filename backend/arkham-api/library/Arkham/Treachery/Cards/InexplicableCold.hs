module Arkham.Treachery.Cards.InexplicableCold (inexplicableCold) where

import Arkham.Card
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InexplicableCold = InexplicableCold TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inexplicableCold :: TreacheryCard InexplicableCold
inexplicableCold = treachery InexplicableCold Cards.inexplicableCold

instance RunMessage InexplicableCold where
  runMessage msg t@(InexplicableCold attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      hand <- filterCards DiscardableCard <$> iid.hand
      if null assets && null hand
        then assignDamage iid attrs n
        else do
          chooseOneM iid do
            targets assets $ toDiscardBy iid attrs
            targets hand $ discardCard iid attrs
            withI18n $ countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
          doStep (n - 1) msg'

      pure t
    _ -> InexplicableCold <$> liftRunMessage msg attrs
