{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scrying where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..))
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype Scrying = Scrying Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

scrying :: AssetId -> Scrying
scrying uuid = Scrying $ (baseAttrs uuid "01061") { assetSlots = [ArcaneSlot] }

instance (IsInvestigator investigator) => HasActions env investigator Scrying where
  getActions i window (Scrying x) = getActions i window x

instance (AssetRunner env) => RunMessage env Scrying where
  runMessage msg a@(Scrying attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId -> do
      let
        attrs' =
          attrs
            & (uses .~ Uses Resource.Charge 3)
            & (abilities
              .~ [mkAbility (AssetSource aid) 1 (ActionAbility 1 Nothing)]
              )
      Scrying <$> runMessage msg attrs'
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId -> do
      locationId <- asks (getId @LocationId iid)
      investigatorIds <- HashSet.toList <$> asks (getSet locationId)
      case assetUses of
        Uses Resource.Charge n -> do
          when
            (n == 1)
            (unshiftMessage (RemoveAbilitiesFrom (AssetSource assetId)))
          unshiftMessage
            (Ask iid
            $ ChooseOne
            $ SearchTopOfDeck iid EncounterDeckTarget 3 [] PutBackInAnyOrder
            : [ SearchTopOfDeck
                  iid
                  (InvestigatorTarget iid')
                  3
                  []
                  PutBackInAnyOrder
              | iid' <- investigatorIds
              ]
            )
          pure $ Scrying $ attrs & uses .~ Uses Resource.Charge (n - 1)
        _ -> pure a
    _ -> Scrying <$> runMessage msg attrs
