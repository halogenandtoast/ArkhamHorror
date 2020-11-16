{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.FrozenInFear where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype FrozenInFear = FrozenInFear Attrs
  deriving newtype (Show, ToJSON, FromJSON)

frozenInFear :: TreacheryId -> a -> FrozenInFear
frozenInFear uuid _ = FrozenInFear $ baseAttrs uuid "01164"

instance HasModifiersFor env FrozenInFear where
  getModifiersFor _ (InvestigatorTarget iid) (FrozenInFear attrs) = pure
    [ ActionCostOf (FirstOneOf [Action.Move, Action.Fight, Action.Evade]) 1
    | iid `elem` treacheryAttachedInvestigator attrs
    ]
  getModifiersFor _ _ _ = pure []

instance HasActions env FrozenInFear where
  getActions i window (FrozenInFear attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env FrozenInFear where
  runMessage msg t@(FrozenInFear attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid)
      FrozenInFear <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    ChooseEndTurn iid | Just iid == treacheryAttachedInvestigator ->
      t <$ unshiftMessage
        (RevelationSkillTest iid (TreacherySource treacheryId) SkillWillpower 3)
    PassedSkillTest _ _ source _ _ | isSource attrs source ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> FrozenInFear <$> runMessage msg attrs
