module Arkham.Types.Treachery.Cards.FrozenInFear where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import qualified Arkham.Types.Action as Action
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype FrozenInFear = FrozenInFear TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFear :: TreacheryId -> a -> FrozenInFear
frozenInFear uuid _ = FrozenInFear $ baseAttrs uuid "01164"

instance HasModifiersFor env FrozenInFear where
  getModifiersFor _ (InvestigatorTarget iid) (FrozenInFear attrs) =
    pure $ toModifiers
      attrs
      [ ActionCostOf (FirstOneOf [Action.Move, Action.Fight, Action.Evade]) 1
      | treacheryOnInvestigator iid attrs
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions env FrozenInFear where
  getActions i window (FrozenInFear attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    ChooseEndTurn iid | treacheryOnInvestigator iid attrs -> t <$ unshiftMessage
      (RevelationSkillTest iid (TreacherySource treacheryId) SkillWillpower 3)
    PassedSkillTest _ _ source _ _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> FrozenInFear <$> runMessage msg attrs
