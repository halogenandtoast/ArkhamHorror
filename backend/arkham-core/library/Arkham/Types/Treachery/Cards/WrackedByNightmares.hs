{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.WrackedByNightmares where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype WrackedByNightmares = WrackedByNightmares Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wrackedByNightmares
  :: TreacheryId -> Maybe InvestigatorId -> WrackedByNightmares
wrackedByNightmares uuid iid =
  WrackedByNightmares $ weaknessAttrs uuid iid "02015"

instance ActionRunner env => HasActions env WrackedByNightmares where
  getActions iid NonFast (WrackedByNightmares Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        treacheryLocation <- asks (getId tormented)
        investigatorLocationId <- asks $ getId @LocationId iid
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

instance (TreacheryRunner env) => RunMessage env WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessage $ AttachTreachery tid (InvestigatorTarget iid)
      WrackedByNightmares
        <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    AttachTreachery tid (InvestigatorTarget iid) | tid == treacheryId -> do
      assetIds <- setToList <$> asks (getSet iid)
      unshiftMessages
        $ AddModifiers
            (InvestigatorTarget iid)
            (TreacherySource tid)
            [ControlledAssetsCannotReady]
        : [ Exhaust (AssetTarget aid) | aid <- assetIds ]
      WrackedByNightmares <$> runMessage msg attrs
    UseCardAbility _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> WrackedByNightmares <$> runMessage msg attrs
