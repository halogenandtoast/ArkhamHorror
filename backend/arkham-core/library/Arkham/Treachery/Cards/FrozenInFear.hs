module Arkham.Treachery.Cards.FrozenInFear where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFear :: TreacheryCard FrozenInFear
frozenInFear = treachery FrozenInFear Cards.frozenInFear

instance HasModifiersFor FrozenInFear where
  getModifiersFor (InvestigatorTarget iid) (FrozenInFear attrs) =
    pure
      $ toModifiers attrs
      $ [ AdditionalActionCostOf (FirstOneOfPerformed [#move, #fight, #evade]) 1
        | treacheryOnInvestigator iid attrs
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities FrozenInFear where
  getAbilities (FrozenInFear a) = [restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds #after You]

instance RunMessage FrozenInFear where
  runMessage msg t@(FrozenInFear attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> FrozenInFear <$> runMessage msg attrs
