module Arkham.Homebrew.AgainstTheWendigo.Treacheries.BlazingAttack (blazingAttack) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype BlazingAttack = BlazingAttack TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blazingAttack :: TreacheryCard BlazingAttack
blazingAttack = treachery BlazingAttack Cards.blazingAttack

instance RunMessage BlazingAttack where
  runMessage msg t@(BlazingAttack attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseSkillM iid [#combat, #agility] \sType ->
        revelationSkillTest sid iid attrs sType (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directDamageAndHorror iid attrs 1 1
      pure t
    _ -> BlazingAttack <$> liftRunMessage msg attrs
