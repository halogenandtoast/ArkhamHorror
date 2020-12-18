{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.GrotesqueStatue4
  ( GrotesqueStatue4(..)
  , grotesqueStatue4
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.ChaosBagStepState

newtype GrotesqueStatue4 = GrotesqueStatue4 Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

grotesqueStatue4 :: AssetId -> GrotesqueStatue4
grotesqueStatue4 uuid =
  GrotesqueStatue4 $ (baseAttrs uuid "01071") { assetSlots = [HandSlot] }

instance HasModifiersFor env GrotesqueStatue4 where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Source -> Window -> Ability
ability attrs source window = (mkAbility
                                (toSource attrs)
                                1
                                (ReactionAbility window
                                $ UseCost (toId attrs) Charge 1
                                )
                              )
  { abilityMetadata = Just (SourceMetadata source)
  , abilityLimit = PerTestOrAbility
  }

instance ActionRunner env => HasActions env GrotesqueStatue4 where
  getActions iid window@(WhenWouldRevealChaosToken source You) (GrotesqueStatue4 a)
    | ownedBy a iid
    = do
      let ability' = (iid, ability a source window)
      unused <- notElem ability' . map unUsedAbility <$> getList ()
      pure [ uncurry ActivateCardAbilityAction ability' | unused ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env GrotesqueStatue4 where
  runMessage msg a@(GrotesqueStatue4 attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      GrotesqueStatue4 <$> runMessage msg (attrs & usesL .~ Uses Charge 4)
    UseCardAbility iid source (Just (SourceMetadata drawSource)) 1
      | isSource attrs source -> do
        when (useCount (assetUses attrs) == 1)
          $ unshiftMessage (Discard (toTarget attrs))
        a <$ unshiftMessage
          (ReplaceCurrentDraw drawSource iid
          $ Choose 1 [Undecided Draw, Undecided Draw] []
          )
    _ -> GrotesqueStatue4 <$> runMessage msg attrs
