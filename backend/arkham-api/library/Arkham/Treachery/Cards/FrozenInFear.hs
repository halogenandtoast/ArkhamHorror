module Arkham.Treachery.Cards.FrozenInFear where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFear :: TreacheryCard FrozenInFear
frozenInFear = treachery FrozenInFear Cards.frozenInFear

instance HasModifiersFor FrozenInFear where
  getModifiersFor (FrozenInFear attrs) =
    inThreatAreaGets attrs [AdditionalActionCostOf (FirstOneOfPerformed [#move, #fight, #evade]) 1]

instance HasAbilities FrozenInFear where
  getAbilities (FrozenInFear a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage FrozenInFear where
  runMessage msg t@(FrozenInFear attrs) = runQueueT $ case msg of
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
    _ -> FrozenInFear <$> liftRunMessage msg attrs
