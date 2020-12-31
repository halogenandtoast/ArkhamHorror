{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.InternalInjury
  ( internalInjury
  , InternalInjury(..)
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype InternalInjury = InternalInjury Attrs
  deriving newtype (Show, ToJSON, FromJSON)

internalInjury :: TreacheryId -> Maybe InvestigatorId -> InternalInjury
internalInjury uuid iid = InternalInjury $ weaknessAttrs uuid iid "02038"

instance HasModifiersFor env InternalInjury where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env InternalInjury where
  getActions iid NonFast (InternalInjury a@Attrs {..}) =
    withTreacheryInvestigator a $ \tormented -> do
      investigatorLocationId <- getId @LocationId iid
      treacheryLocation <- getId tormented
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 2))
        | treacheryLocation == investigatorLocationId
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env InternalInjury where
  runMessage msg t@(InternalInjury attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    EndTurn iid | InvestigatorTarget iid `elem` treacheryAttachedTarget ->
      t <$ unshiftMessage (InvestigatorDirectDamage iid (toSource attrs) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> InternalInjury <$> runMessage msg attrs
