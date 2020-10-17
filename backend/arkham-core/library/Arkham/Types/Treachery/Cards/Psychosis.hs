{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Psychosis where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Psychosis = Psychosis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

psychosis :: TreacheryId -> Maybe InvestigatorId -> Psychosis
psychosis uuid iid = Psychosis $ weaknessAttrs uuid iid "01099"

instance ActionRunner env => HasActions env Psychosis where
  getActions iid NonFast (Psychosis Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        investigatorLocationId <- asks $ getId @LocationId iid
        treacheryLocation <- asks (getId tormented)
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

instance (TreacheryRunner env) => RunMessage env Psychosis where
  runMessage msg t@(Psychosis attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage $ AttachTreachery tid (InvestigatorTarget iid)
      Psychosis <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    After (InvestigatorTakeDamage iid _ _ n)
      | Just iid == treacheryAttachedInvestigator && n > 0
      -> t <$ unshiftMessage
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Psychosis <$> runMessage msg attrs
