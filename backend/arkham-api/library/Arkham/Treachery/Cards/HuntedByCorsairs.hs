module Arkham.Treachery.Cards.HuntedByCorsairs (huntedByCorsairs) where

import Arkham.Ability
import Arkham.Helpers.Act
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntedByCorsairs = HuntedByCorsairs TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedByCorsairs :: TreacheryCard HuntedByCorsairs
huntedByCorsairs = treachery HuntedByCorsairs Cards.huntedByCorsairs

instance HasAbilities HuntedByCorsairs where
  getAbilities (HuntedByCorsairs attrs) = case attrs.attached of
    Just (ActTarget aid) ->
      [ mkAbility attrs 1 $ forced $ ActAdvances #when (ActWithId aid)
      , skillTestAbility $ mkAbility (proxied (ActWithId aid) attrs) 2 actionAbility
      ]
    _ -> []

instance RunMessage HuntedByCorsairs where
  runMessage msg t@(HuntedByCorsairs attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      attachTreachery attrs =<< getCurrentAct
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator $ assignDamageTo (attrs.ability 1) 2
      pure t
    UseThisAbility iid source@(isProxySource attrs -> True) 2 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (AbilitySource source 2) attrs [#intellect, #agility] (Fixed 4)
      pure t
    PassedThisSkillTest iid source@(isProxyAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid source attrs
      pure t
    _ -> HuntedByCorsairs <$> liftRunMessage msg attrs
