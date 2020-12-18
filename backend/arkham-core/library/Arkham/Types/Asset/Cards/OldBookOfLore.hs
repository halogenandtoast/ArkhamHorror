{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.OldBookOfLore where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype OldBookOfLore = OldBookOfLore Attrs
  deriving newtype (Show, ToJSON, FromJSON)

oldBookOfLore :: AssetId -> OldBookOfLore
oldBookOfLore uuid =
  OldBookOfLore $ (baseAttrs uuid "01031") { assetSlots = [HandSlot] }

instance HasModifiersFor env OldBookOfLore where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env OldBookOfLore where
  getActions iid NonFast (OldBookOfLore a) | ownedBy a iid = do
    canAffordActions <- getCanAffordCost
      iid
      (toSource a)
      (ActionCost 1 Nothing (assetTraits a))
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
      | not (assetExhausted a) && canAffordActions
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env OldBookOfLore where
  runMessage msg (OldBookOfLore attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      unshiftMessage
        (chooseOne
          iid
          [ SearchTopOfDeck iid' (InvestigatorTarget iid') 3 [] ShuffleBackIn
          | iid' <- investigatorIds
          ]
        )
      pure $ OldBookOfLore $ attrs & exhaustedL .~ True
    _ -> OldBookOfLore <$> runMessage msg attrs
