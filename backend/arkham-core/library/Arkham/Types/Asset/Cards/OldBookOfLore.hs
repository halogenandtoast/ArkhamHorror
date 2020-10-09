{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.OldBookOfLore where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype OldBookOfLore = OldBookOfLore Attrs
  deriving newtype (Show, ToJSON, FromJSON)

oldBookOfLore :: AssetId -> OldBookOfLore
oldBookOfLore uuid =
  OldBookOfLore $ (baseAttrs uuid "01031") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator OldBookOfLore where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator OldBookOfLore where
  getActions i NonFast (OldBookOfLore a) | ownedBy a i = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | not (assetExhausted a) && hasActionsRemaining i Nothing (assetTraits a)
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env OldBookOfLore where
  runMessage msg (OldBookOfLore attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      investigatorIds <- asks $ setToList . getSet locationId
      unshiftMessage
        (chooseOne
          iid
          [ SearchTopOfDeck iid' (InvestigatorTarget iid') 3 [] ShuffleBackIn
          | iid' <- investigatorIds
          ]
        )
      pure $ OldBookOfLore $ attrs & exhausted .~ True
    _ -> OldBookOfLore <$> runMessage msg attrs
