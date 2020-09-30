{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.WrackedByNightmares where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype WrackedByNightmares = WrackedByNightmares Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wrackedByNightmares :: TreacheryId -> Maybe InvestigatorId -> WrackedByNightmares
wrackedByNightmares uuid iid = WrackedByNightmares $ weaknessAttrs uuid iid "02015"

instance (ActionRunner env investigator) => HasActions env investigator WrackedByNightmares where
  getActions i NonFast (WrackedByNightmares Attrs {..}) =
    case treacheryAttachedInvestigator of
      Nothing -> pure []
      Just tormented -> do
        treacheryLocation <- asks (getId tormented)
        pure
          [ ActivateCardAbilityAction
              (getId () i)
              (mkAbility
                (TreacherySource treacheryId)
                1
                (ActionAbility 2 Nothing)
              )
          | treacheryLocation == locationOf i
          ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      assetIds <- setToList <$> asks (getSet iid)
      unshiftMessages [Exhaust (AssetTarget aid) | aid <- assetIds]
      WrackedByNightmares <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    AttachTreachery tid (InvestigatorTarget iid) | tid == treacheryId -> do
      unshiftMessage
        (AddModifiers
          (InvestigatorTarget iid)
          (TreacherySource tid)
          [ControlledAssetsCannotReady]
        )
      WrackedByNightmares <$> runMessage msg attrs
    UseCardAbility _ _ (TreacherySource tid) _ 1 | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> WrackedByNightmares <$> runMessage msg attrs
