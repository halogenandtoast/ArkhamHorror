{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.ExtraAmmunition1 where

import Arkham.Json
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.LocationId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Control.Monad.Extra hiding (filterM)
import Lens.Micro
import qualified Data.HashSet as HashSet

import ClassyPrelude

newtype ExtraAmmunition1 = ExtraAmmunition1 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

extraAmmunition1 :: InvestigatorId -> EventId -> ExtraAmmunition1
extraAmmunition1 iid uuid = ExtraAmmunition1 $ baseAttrs iid uuid "01026"

instance HasActions env investigator ExtraAmmunition1 where
  getActions i window (ExtraAmmunition1 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env ExtraAmmunition1 where
  runMessage msg (ExtraAmmunition1 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      locationId <- asks (getId @LocationId iid)
      investigatorIds <- HashSet.toList <$> asks (getSet @InvestigatorId locationId)
      assetIds <- concatForM investigatorIds (asks . (HashSet.toList .) . getSet)
      firearmAssetids <- flip filterM assetIds $ \assetId -> do
        traits <- HashSet.toList <$> asks (getSet assetId)
        pure $ Firearm `elem` traits
      unshiftMessage (Ask iid $ ChooseOne [ Run [ AddUses (AssetTarget aid) Ammo 3, Discard (EventTarget eventId) ] | aid <- firearmAssetids ])
      ExtraAmmunition1 <$> runMessage msg (attrs & resolved .~ True)
    _ -> ExtraAmmunition1 <$> runMessage msg attrs
