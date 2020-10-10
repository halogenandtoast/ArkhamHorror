{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Scrying where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype Scrying = Scrying Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

scrying :: AssetId -> Scrying
scrying uuid = Scrying $ (baseAttrs uuid "01061") { assetSlots = [ArcaneSlot] }

instance HasModifiersFor env investigator Scrying where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Scrying where
  getActions i NonFast (Scrying a) | ownedBy a i && not (assetExhausted a) =
    pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
      | useCount (assetUses a) > 0 && hasActionsRemaining
        i
        Nothing
        (assetTraits a)
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Scrying where
  runMessage msg (Scrying attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Scrying <$> runMessage msg (attrs & uses .~ Uses Resource.Charge 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      targets <- asks $ map InvestigatorTarget . setToList . getSet locationId
      unshiftMessage
        (chooseOne iid
        $ SearchTopOfDeck iid EncounterDeckTarget 3 [] PutBackInAnyOrder
        : [ SearchTopOfDeck iid target 3 [] PutBackInAnyOrder
          | target <- targets
          ]
        )
      pure $ Scrying $ attrs & uses %~ Resource.use & exhausted .~ True
    _ -> Scrying <$> runMessage msg attrs
