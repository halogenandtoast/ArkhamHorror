module Arkham.Treachery.Cards.FrozenInFear where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFear :: TreacheryCard FrozenInFear
frozenInFear = treachery FrozenInFear Cards.frozenInFear

instance HasModifiersFor FrozenInFear where
  getModifiersFor (InvestigatorTarget iid) (FrozenInFear attrs) =
    modified
      attrs
      [ AdditionalActionCostOf (FirstOneOfPerformed [#move, #fight, #evade]) 1
      | treacheryInThreatArea iid attrs
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities FrozenInFear where
  getAbilities (FrozenInFear a) = [restrictedAbility a 1 (InThreatAreaOf You) $ forced $ TurnEnds #after You]

instance RunMessage FrozenInFear where
  runMessage msg t@(FrozenInFear attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> FrozenInFear <$> runMessage msg attrs
