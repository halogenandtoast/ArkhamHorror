{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Haunted where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Haunted = Haunted Attrs
  deriving newtype (Show, ToJSON, FromJSON)

haunted :: TreacheryId -> Maybe InvestigatorId -> Haunted
haunted uuid iid = Haunted $ weaknessAttrs uuid iid "01098"

instance HasModifiersFor env Haunted where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Haunted where
  getActions iid NonFast (Haunted Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        investigatorLocationId <- asks $ getId @LocationId iid
        treacheryLocation <- asks $ getId tormented
        pure
          [ ActivateCardAbilityAction
              iid
              (mkAbility
                (TreacherySource treacheryId)
                1
                (ActionAbility 2 Nothing)
              )
          | treacheryLocation == investigatorLocationId
          ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Haunted where
  runMessage msg t@(Haunted attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid)
      Haunted <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    AttachTreachery tid (InvestigatorTarget iid) | tid == treacheryId -> do
      unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (TreacherySource tid)
          [AnySkillValue (-1)]
        )
      Haunted <$> runMessage msg attrs
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Haunted <$> runMessage msg attrs
