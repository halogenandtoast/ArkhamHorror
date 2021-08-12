module Arkham.Types.Treachery.Cards.FrozenInFear where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
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

instance (TreacheryRunner env) => RunMessage env FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery treacheryId $ InvestigatorTarget iid)
    ChooseEndTurn iid | treacheryOnInvestigator iid attrs -> t <$ push
      (RevelationSkillTest iid (TreacherySource treacheryId) SkillWillpower 3)
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ push (Discard $ toTarget attrs)
    _ -> FrozenInFear <$> runMessage msg attrs
