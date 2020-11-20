{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.ExtraAmmunition1 where

import Arkham.Import

import Arkham.Types.Asset.Uses
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Trait
import Control.Monad.Extra hiding (filterM)

newtype ExtraAmmunition1 = ExtraAmmunition1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

extraAmmunition1 :: InvestigatorId -> EventId -> ExtraAmmunition1
extraAmmunition1 iid uuid = ExtraAmmunition1 $ baseAttrs iid uuid "01026"

instance HasModifiersFor env ExtraAmmunition1 where
  getModifiersFor = noModifiersFor

instance HasActions env ExtraAmmunition1 where
  getActions i window (ExtraAmmunition1 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env ExtraAmmunition1 where
  runMessage msg e@(ExtraAmmunition1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList @InvestigatorId locationId
      assetIds <- concatForM investigatorIds getSetList
      firearmAssetids <- flip filterM assetIds $ \assetId -> do
        elem Firearm <$> getSetList assetId
      e <$ unshiftMessages
        [ chooseOne
          iid
          [ Run
              [AddUses (AssetTarget aid) Ammo 3, Discard (EventTarget eventId)]
          | aid <- firearmAssetids
          ]
        , Discard (toTarget attrs)
        ]
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
