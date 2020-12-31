{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.Flashlight
  ( Flashlight(..)
  , flashlight
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype Flashlight = Flashlight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

flashlight :: AssetId -> Flashlight
flashlight uuid =
  Flashlight $ (baseAttrs uuid "01087") { assetSlots = [HandSlot] }

instance HasModifiersFor env Flashlight where
  getModifiersFor = noModifiersFor

investigateAbility :: Attrs -> Ability
investigateAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility (Just Action.Investigate) (ActionCost 1))

instance ActionRunner env => HasActions env Flashlight where
  getActions iid window (Flashlight a) | ownedBy a iid = do
    investigateAvailable <- hasInvestigateActions iid window
    pure
      [ ActivateCardAbilityAction iid (investigateAbility a)
      | useCount (assetUses a) > 0 && investigateAvailable
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Flashlight where
  runMessage msg (Flashlight attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Flashlight <$> runMessage msg (attrs & usesL .~ Uses Resource.Supply 3)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      lid <- getId iid
      unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers $ toModifiers attrs [ShroudModifier (-2)])
          source
          (LocationTarget lid)
        , Investigate iid lid source SkillIntellect False
        ]
      pure $ Flashlight $ attrs & usesL %~ Resource.use
    _ -> Flashlight <$> runMessage msg attrs
