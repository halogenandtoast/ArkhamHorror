{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.TheNecronomiconAdvanced where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

theNecronomiconAdvanced :: AssetId -> TheNecronomiconAdvanced
theNecronomiconAdvanced uuid =
  TheNecronomiconAdvanced $ baseAttrs uuid "80003" $ do
    slots .= [HandSlot]
    horror ?= 3
    canLeavePlayByNormalMeans .= False

instance HasModifiersFor env TheNecronomiconAdvanced where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomiconAdvanced a) = pure
    [ ForcedTokenChange ElderSign [Cultist, Tablet, ElderThing]
    | ownedBy a iid
    ]
  getModifiersFor _ _ _ = pure []

instance HasActions env TheNecronomiconAdvanced where
  getActions iid NonFast (TheNecronomiconAdvanced a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | fromJustNote "Must be set" (assetHorror a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ InvestigatorDamage iid source 0 1
      if fromJustNote "Must be set" (assetHorror attrs) == 1
        then a <$ unshiftMessage (Discard (toTarget attrs))
        else pure $ TheNecronomiconAdvanced
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror attrs })
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
