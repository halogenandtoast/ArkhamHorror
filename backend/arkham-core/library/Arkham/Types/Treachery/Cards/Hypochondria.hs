{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Hypochondria where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype Hypochondria = Hypochondria Attrs
  deriving newtype (Show, ToJSON, FromJSON)

hypochondria :: TreacheryId -> Maybe InvestigatorId -> Hypochondria
hypochondria uuid iid = Hypochondria $ weaknessAttrs uuid iid "01100"

instance HasModifiersFor env Hypochondria where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Hypochondria where
  getActions iid NonFast (Hypochondria a@Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        treacheryLocation <- getId tormented
        investigatorLocationId <- getId @LocationId iid
        canAffordActions <- getCanAffordCost
          iid
          (toSource a)
          (ActionCost 2 Nothing treacheryTraits)
        pure
          [ ActivateCardAbilityAction
              iid
              (mkAbility
                (TreacherySource treacheryId)
                1
                (ActionAbility 2 Nothing)
              )
          | canAffordActions && treacheryLocation == investigatorLocationId
          ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Hypochondria where
  runMessage msg t@(Hypochondria attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid)
      Hypochondria <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    After (InvestigatorTakeDamage iid _ n _)
      | Just iid == treacheryAttachedInvestigator && n > 0
      -> t <$ unshiftMessage
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 0 1)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Hypochondria <$> runMessage msg attrs
