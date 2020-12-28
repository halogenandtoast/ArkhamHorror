{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Treachery.Cards.InternalInjury
  ( internalInjury
  , InternalInjury(..)
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
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
      canActivate <- getCanAffordCost
        iid
        (toSource a)
        (ActionCost 2 Nothing treacheryTraits)
      investigatorLocationId <- getId @LocationId iid
      treacheryLocation <- getId tormented
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (TreacherySource treacheryId) 1 (ActionAbility 2 Nothing)
            )
        | treacheryLocation == investigatorLocationId && canActivate
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env InternalInjury where
  runMessage msg t@(InternalInjury attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    EndTurn iid | InvestigatorTarget iid `elem` treacheryAttachedTarget ->
      t <$ unshiftMessage (InvestigatorDirectDamage iid (toSource attrs) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> InternalInjury <$> runMessage msg attrs
