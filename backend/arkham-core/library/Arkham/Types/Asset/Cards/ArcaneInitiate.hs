{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneInitiate where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import Lens.Micro

newtype ArcaneInitiate = ArcaneInitiate Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneInitiate :: AssetId -> ArcaneInitiate
arcaneInitiate uuid = ArcaneInitiate $ (baseAttrs uuid "01063")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance HasModifiersFor env investigator ArcaneInitiate where
  getModifiersFor _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator ArcaneInitiate where
  getActions i window (ArcaneInitiate Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (FastAbility window))
      | not assetExhausted
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ArcaneInitiate where
  runMessage msg (ArcaneInitiate attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      ArcaneInitiate <$> runMessage msg (attrs & doom +~ 1) -- TODO: this triggers multiple time due to having to discard other assets
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      unshiftMessage
        (Ask iid
        $ ChooseOne
            [ SearchTopOfDeck
                iid
                (InvestigatorTarget iid)
                3
                [Spell]
                ShuffleBackIn
            ]
        )
      pure $ ArcaneInitiate $ attrs & exhausted .~ True
    _ -> ArcaneInitiate <$> runMessage msg attrs
