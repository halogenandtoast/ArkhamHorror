module Arkham.Homebrew.CircusExMortis.Treacheries.DestructiveImpulses (destructiveImpulses) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (..), hasVice)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype DestructiveImpulses = DestructiveImpulses TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destructiveImpulses :: TreacheryCard DestructiveImpulses
destructiveImpulses = treachery DestructiveImpulses Cards.destructiveImpulses

instance RunMessage DestructiveImpulses where
  runMessage msg t@(DestructiveImpulses attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      vice <- hasVice iid Violence
      chooseNM iid (if vice then 2 else 1) $ withI18n do
        countVar 1 $ labeled "loseActions" $ loseActions iid attrs 1
        countVar 2 $ labeled "takeDamage" $ assignDamage iid attrs 2
        chooseTest #agility 3 $ revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> DestructiveImpulses <$> liftRunMessage msg attrs
