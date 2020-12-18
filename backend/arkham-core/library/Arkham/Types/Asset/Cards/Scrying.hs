{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.Scrying
  ( Scrying(..)
  , scrying
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses

newtype Scrying = Scrying Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

scrying :: AssetId -> Scrying
scrying uuid = Scrying $ (baseAttrs uuid "01061") { assetSlots = [ArcaneSlot] }

instance HasModifiersFor env Scrying where
  getModifiersFor = noModifiersFor

instance HasActions env Scrying where
  getActions iid NonFast (Scrying a) | ownedBy a iid && not (assetExhausted a) =
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility Nothing
            $ Costs [ActionCost 1, UseCost (toId a) Charge 1]
            )
          )
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Scrying where
  runMessage msg (Scrying attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Scrying <$> runMessage msg (attrs & usesL .~ Uses Charge 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- getId @LocationId iid
      targets <- map InvestigatorTarget <$> getSetList locationId
      unshiftMessage
        (chooseOne iid
        $ SearchTopOfDeck iid EncounterDeckTarget 3 [] PutBackInAnyOrder
        : [ SearchTopOfDeck iid target 3 [] PutBackInAnyOrder
          | target <- targets
          ]
        )
      pure $ Scrying $ attrs & exhaustedL .~ True
    _ -> Scrying <$> runMessage msg attrs
