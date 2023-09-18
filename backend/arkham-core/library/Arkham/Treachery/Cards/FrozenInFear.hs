module Arkham.Treachery.Cards.FrozenInFear where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Timing qualified as Timing
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
      $ [ ActionCostOf (FirstOneOfPerformed [Action.Move, Action.Fight, Action.Evade]) 1
        | treacheryOnInvestigator iid attrs
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities FrozenInFear where
  getAbilities (FrozenInFear a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds Timing.After You
    ]

instance RunMessage FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery treacheryId $ InvestigatorTarget iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ revelationSkillTest iid (toAbilitySource attrs 1) #willpower 3
      pure t
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure t
    _ -> FrozenInFear <$> runMessage msg attrs
