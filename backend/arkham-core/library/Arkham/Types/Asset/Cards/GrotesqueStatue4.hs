{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.GrotesqueStatue4 where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource
import Arkham.Types.AssetId
import Arkham.Types.ChaosBagStepState
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype GrotesqueStatue4 = GrotesqueStatue4 Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

grotesqueStatue4 :: AssetId -> GrotesqueStatue4
grotesqueStatue4 uuid =
  GrotesqueStatue4 $ (baseAttrs uuid "01071") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator GrotesqueStatue4 where
  getModifiersFor _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator GrotesqueStatue4 where
  getActions i window@(WhenWouldRevealChaosToken source You) (GrotesqueStatue4 Attrs {..})
    | Just (getId () i) == assetInvestigator
    = do
      let
        ability = (mkAbility (AssetSource assetId) 1 (ReactionAbility window))
          { abilityMetadata = Just (SourceMetadata source)
          , abilityLimit = PerTestOrAbility
          }
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction (getId () i) ability
        | useCount assetUses
          > 0
          && (getId () i, ability)
          `notElem` usedAbilities
        ]
  getActions _ _ _ = pure []


instance (AssetRunner env) => RunMessage env GrotesqueStatue4 where
  runMessage msg a@(GrotesqueStatue4 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      GrotesqueStatue4
        <$> runMessage msg (attrs & uses .~ Uses Resource.Charge 4)
    UseCardAbility iid _ (AssetSource aid) (Just (SourceMetadata source)) 1
      | aid == assetId -> case assetUses of
        Uses Resource.Charge n -> do
          when (n == 1) $ unshiftMessage (Discard (AssetTarget aid))
          unshiftMessage
            (ReplaceCurrentDraw
              source
              iid
              (Choose 1 [Undecided Draw, Undecided Draw] [])
            )
          pure $ GrotesqueStatue4 $ attrs & uses .~ Uses Resource.Charge (n - 1)
        _ -> pure a
    _ -> GrotesqueStatue4 <$> runMessage msg attrs
