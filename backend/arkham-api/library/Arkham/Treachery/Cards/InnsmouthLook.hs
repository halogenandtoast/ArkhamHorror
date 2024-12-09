module Arkham.Treachery.Cards.InnsmouthLook (innsmouthLook, InnsmouthLook (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InnsmouthLook = InnsmouthLook TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthLook :: TreacheryCard InnsmouthLook
innsmouthLook = treachery InnsmouthLook Cards.innsmouthLook

instance HasModifiersFor InnsmouthLook where
  getModifiersFor (InnsmouthLook a) =
    inThreatAreaGets a [SkillModifier #intellect (-1), SanityModifier (-1), AddTrait DeepOne]

instance HasAbilities InnsmouthLook where
  getAbilities (InnsmouthLook a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility]

instance RunMessage InnsmouthLook where
  runMessage msg t@(InnsmouthLook attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> InnsmouthLook <$> liftRunMessage msg attrs
