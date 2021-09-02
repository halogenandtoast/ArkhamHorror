module Arkham.Types.Treachery.Cards.FrozenInFear where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFear :: TreacheryCard FrozenInFear
frozenInFear = treachery FrozenInFear Cards.frozenInFear

instance HasModifiersFor env FrozenInFear where
  getModifiersFor _ (InvestigatorTarget iid) (FrozenInFear attrs) =
    pure $ toModifiers
      attrs
      [ ActionCostOf (FirstOneOf [Action.Move, Action.Fight, Action.Evade]) 1
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities FrozenInFear where
  getAbilities (FrozenInFear a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $ ForcedAbility $ TurnEnds
        Timing.After
        You
    ]

instance TreacheryRunner env => RunMessage env FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    _ -> FrozenInFear <$> runMessage msg attrs
