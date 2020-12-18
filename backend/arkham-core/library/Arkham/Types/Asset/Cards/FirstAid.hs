{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses

newtype FirstAid = FirstAid Attrs
  deriving newtype (Show, ToJSON, FromJSON)

firstAid :: AssetId -> FirstAid
firstAid uuid = FirstAid $ baseAttrs uuid "01019"

instance HasModifiersFor env FirstAid where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (toId attrs) Supply 1])

instance HasActions env FirstAid where
  getActions iid NonFast (FirstAid a) =
    pure [ ActivateCardAbilityAction iid (ability a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env FirstAid where
  runMessage msg a@(FirstAid attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId ->
      FirstAid <$> runMessage msg (attrs & usesL .~ Uses Supply 3)
    UseCardAbility iid (AssetSource aid) _ 1 | aid == assetId -> do
      lid <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList lid
      a <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              target
              [chooseOne iid [HealDamage target 1, HealHorror target 1]]
          | target <- investigatorTargets
          ]
        )
    _ -> FirstAid <$> runMessage msg attrs
