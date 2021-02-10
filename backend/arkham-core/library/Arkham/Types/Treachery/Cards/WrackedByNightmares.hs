module Arkham.Types.Treachery.Cards.WrackedByNightmares
  ( WrackedByNightmares(..)
  , wrackedByNightmares
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype WrackedByNightmares = WrackedByNightmares TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByNightmares
  :: TreacheryId -> Maybe InvestigatorId -> WrackedByNightmares
wrackedByNightmares uuid iid =
  WrackedByNightmares $ weaknessAttrs uuid iid "02015"

instance HasModifiersFor env WrackedByNightmares where
  getModifiersFor _ (InvestigatorTarget iid) (WrackedByNightmares attrs) =
    pure $ toModifiers
      attrs
      [ ControlledAssetsCannotReady | treacheryOnInvestigator iid attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env WrackedByNightmares where
  getActions iid NonFast (WrackedByNightmares a@TreacheryAttrs {..}) =
    withTreacheryInvestigator a $ \tormented -> do
      treacheryLocation <- getId tormented
      investigatorLocationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 2))
        | treacheryLocation == investigatorLocationId
        ]
  getActions _ _ _ = pure []

instance TreacheryRunner env => RunMessage env WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ unshiftMessage (AttachTreachery treacheryId $ InvestigatorTarget iid)
    AttachTreachery tid (InvestigatorTarget iid) | tid == treacheryId -> do
      assetIds <- getSetList iid
      unshiftMessages [ Exhaust (AssetTarget aid) | aid <- assetIds ]
      WrackedByNightmares <$> runMessage msg attrs
    UseCardAbility _ (TreacherySource tid) _ 1 _ | tid == treacheryId ->
      t <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    _ -> WrackedByNightmares <$> runMessage msg attrs
