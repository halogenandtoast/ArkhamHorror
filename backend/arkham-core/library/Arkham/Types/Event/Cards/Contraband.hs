module Arkham.Types.Event.Cards.Contraband
  ( contraband
  , Contraband(..)
  ) where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Asset.Uses
import Arkham.Types.Event.Attrs

newtype Contraband = Contraband EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contraband :: InvestigatorId -> EventId -> Contraband
contraband iid uuid = Contraband $ baseAttrs iid uuid "02109"

instance HasActions env Contraband where
  getActions iid window (Contraband attrs) = getActions iid window attrs

instance HasModifiersFor env Contraband where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasId LocationId env InvestigatorId
  , HasSet InvestigatorId env LocationId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasCount UsesCount env AssetId
  )
  => RunMessage env Contraband where
  runMessage msg e@(Contraband attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList @InvestigatorId locationId
      ammoAssets <- concat
        <$> for investigatorIds (getSetList @AssetId . (, Ammo))

      ammoAssetsWithUseCount <- map (\(c, aid) -> (Ammo, c, aid))
        <$> for ammoAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      supplyAssets <- concat
        <$> for investigatorIds (getSetList @AssetId . (, Supply))

      supplyAssetsWithUseCount <- map (\(c, aid) -> (Supply, c, aid))
        <$> for supplyAssets (\aid -> (, aid) . unUsesCount <$> getCount aid)

      e <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              (AssetTarget assetId)
              [AddUses (AssetTarget assetId) useType' assetUseCount]
          | (useType', assetUseCount, assetId) <-
            ammoAssetsWithUseCount <> supplyAssetsWithUseCount
          ]
        )
    _ -> Contraband <$> runMessage msg attrs
