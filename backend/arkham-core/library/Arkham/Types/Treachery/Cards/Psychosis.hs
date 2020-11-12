{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.Psychosis where

import Arkham.Import

import Arkham.Types.Action (Action)
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Psychosis = Psychosis Attrs
  deriving newtype (Show, ToJSON, FromJSON)

psychosis :: TreacheryId -> Maybe InvestigatorId -> Psychosis
psychosis uuid iid = Psychosis $ weaknessAttrs uuid iid "01099"

instance HasModifiersFor env Psychosis where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Psychosis where
  getActions iid NonFast (Psychosis Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        canActivate <- asks $ (>= 2) . unActionRemainingCount . getCount
          (iid, Nothing :: Maybe Action, setToList treacheryTraits)
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
          | canActivate && treacheryLocation == investigatorLocationId
          ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env Psychosis where
  runMessage msg t@(Psychosis attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessage $ AttachTreachery treacheryId (InvestigatorTarget iid)
      Psychosis <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    After (InvestigatorTakeDamage iid _ _ n)
      | Just iid == treacheryAttachedInvestigator && n > 0
      -> t <$ unshiftMessage
        (InvestigatorDirectDamage iid (TreacherySource treacheryId) 1 0)
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> Psychosis <$> runMessage msg attrs
