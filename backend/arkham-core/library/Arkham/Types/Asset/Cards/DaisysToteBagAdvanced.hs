{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DaisysToteBagAdvanced
  ( daisysToteBagAdvanced
  , DaisysToteBagAdvanced(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Trait

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced Attrs
  deriving newtype (Show, ToJSON, FromJSON)

daisysToteBagAdvanced :: AssetId -> DaisysToteBagAdvanced
daisysToteBagAdvanced uuid = DaisysToteBagAdvanced $ baseAttrs uuid "90002"

instance HasSet Trait (InvestigatorId, CardId) env => HasActions env DaisysToteBagAdvanced where
  getActions iid window@(WhenPlayCard You cardId) (DaisysToteBagAdvanced a)
    | ownedBy a iid = do
      isTome <- asks $ elem Tome . getSet @Trait (iid, cardId)
      let
        ability = (mkAbility (toSource a) 1 (ReactionAbility window))
          { abilityMetadata = Just (TargetMetadata $ CardIdTarget cardId)
          }
      pure
        [ ActivateCardAbilityAction iid ability
        | not (assetExhausted a) && isTome
        ]
  getActions iid window (DaisysToteBagAdvanced attrs) =
    getActions iid window attrs

instance HasModifiersFor env DaisysToteBagAdvanced where
  getModifiersFor _ (InvestigatorTarget iid) (DaisysToteBagAdvanced a)
    | ownedBy a iid = pure [CanBecomeFast (Just AssetType, [Tome])]
  getModifiersFor _ _ _ = pure []

slot :: Attrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance (HasQueue env, HasModifiers env InvestigatorId) => RunMessage env DaisysToteBagAdvanced where
  runMessage msg (DaisysToteBagAdvanced attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessages $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBagAdvanced <$> runMessage msg attrs
    UseCardAbility iid source (Just (TargetMetadata (CardIdTarget cardId))) 1
      | isSource attrs source -> do
        unshiftMessage (ChangeCardToFast iid cardId)
        pure . DaisysToteBagAdvanced $ attrs & exhausted .~ True
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs
