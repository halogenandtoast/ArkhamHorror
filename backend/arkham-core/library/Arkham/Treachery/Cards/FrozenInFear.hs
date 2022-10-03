module Arkham.Treachery.Cards.FrozenInFear where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Runner
import Arkham.Treachery.Helpers

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFear :: TreacheryCard FrozenInFear
frozenInFear = treachery FrozenInFear Cards.frozenInFear

instance HasModifiersFor FrozenInFear where
  getModifiersFor (InvestigatorTarget iid) (FrozenInFear attrs) =
    pure $ toModifiers
      attrs
      [ ActionCostOf (FirstOneOf [Action.Move, Action.Fight, Action.Evade]) 1
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities FrozenInFear where
  getAbilities (FrozenInFear a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds
        Timing.After
        You
    ]

instance RunMessage FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    _ -> FrozenInFear <$> runMessage msg attrs
