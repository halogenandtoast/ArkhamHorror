{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.TheNecronomicon where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype TheNecronomicon = TheNecronomicon Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

theNecronomicon :: AssetId -> TheNecronomicon
theNecronomicon uuid = TheNecronomicon $ baseAttrs uuid "01009" $ do
  slots .= [HandSlot]
  horror ?= 3
  canLeavePlayByNormalMeans .= False

instance HasModifiersFor env TheNecronomicon where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomicon a) =
    pure [ ForcedTokenChange ElderSign [AutoFail] | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env TheNecronomicon where
  getActions iid NonFast (TheNecronomicon a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | fromJustNote "Must be set" (assetHorror a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ InvestigatorDamage iid source 0 1
      if fromJustNote "Must be set" (assetHorror attrs) == 1
        then a <$ unshiftMessage (Discard (toTarget attrs))
        else pure $ TheNecronomicon
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror attrs })
    _ -> TheNecronomicon <$> runMessage msg attrs
