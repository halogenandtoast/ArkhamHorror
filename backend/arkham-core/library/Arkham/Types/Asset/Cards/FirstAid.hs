{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype FirstAid = FirstAid Attrs
  deriving newtype (Show, ToJSON, FromJSON)

firstAid :: AssetId -> FirstAid
firstAid uuid = FirstAid $ baseAttrs uuid "01019" $ pure ()

instance HasModifiersFor env FirstAid where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance ActionRunner env => HasActions env FirstAid where
  getActions iid window (FirstAid a) | ownedBy a iid = do
    investigateAvailable <- hasInvestigateActions iid window
    pure
      [ ActivateCardAbilityAction iid (ability a)
      | useCount (assetUses a) > 0 && investigateAvailable
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FirstAid where
  runMessage msg (FirstAid attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      FirstAid <$> runMessage msg (attrs & uses .~ Uses Resource.Supply 3)
    UseCardAbility iid (AssetSource aid) _ 1 | aid == assetId -> do
      lid <- asks $ getId @LocationId iid
      investigatorTargets <-
        asks $ map InvestigatorTarget . setToList . getSet lid
      unshiftMessage $ chooseOne
        iid
        [ TargetLabel
            target
            [chooseOne iid [HealDamage target 1, HealHorror target 1]]
        | target <- investigatorTargets
        ]
      pure $ FirstAid $ attrs & uses %~ Resource.use
    _ -> FirstAid <$> runMessage msg attrs
