{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Atychiphobia
  ( atychiphobia
  , Atychiphobia(..)
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Atychiphobia = Atychiphobia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

atychiphobia :: TreacheryId -> Maybe InvestigatorId -> Atychiphobia
atychiphobia uuid iid = Atychiphobia $ weaknessAttrs uuid iid "60504"

instance HasModifiersFor env Atychiphobia where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Atychiphobia where
  getActions iid NonFast (Atychiphobia Attrs {..}) =
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

instance (TreacheryRunner env) => RunMessage env Atychiphobia where
  runMessage msg t@(Atychiphobia attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid)
      Atychiphobia <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    FailedSkillTest iid _ _ _ _ | Just iid == treacheryAttachedInvestigator ->
      t <$ unshiftMessage
        (InvestigatorAssignDamage iid (TreacherySource treacheryId) 0 1)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Atychiphobia <$> runMessage msg attrs
