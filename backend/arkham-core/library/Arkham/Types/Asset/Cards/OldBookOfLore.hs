{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.OldBookOfLore where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype OldBookOfLore = OldBookOfLore Attrs
  deriving newtype (Show, ToJSON, FromJSON)

oldBookOfLore :: AssetId -> OldBookOfLore
oldBookOfLore uuid =
  OldBookOfLore $ (baseAttrs uuid "01031") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator OldBookOfLore where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator OldBookOfLore where
  getActions i NonFast (OldBookOfLore Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      | not assetExhausted && hasActionsRemaining i Nothing assetTraits
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env OldBookOfLore where
  runMessage msg (OldBookOfLore attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      locationId <- asks (getId @LocationId iid)
      investigatorIds <- HashSet.toList <$> asks (getSet locationId)
      unshiftMessage
        (Ask iid $ ChooseOne
          [ SearchTopOfDeck iid' (InvestigatorTarget iid') 3 [] ShuffleBackIn
          | iid' <- investigatorIds
          ]
        )
      pure $ OldBookOfLore $ attrs & exhausted .~ True
    _ -> OldBookOfLore <$> runMessage msg attrs
