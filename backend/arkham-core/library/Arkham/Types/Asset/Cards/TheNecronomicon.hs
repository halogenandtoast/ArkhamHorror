{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.TheNecronomicon where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import qualified Arkham.Types.Token as Token

newtype TheNecronomicon = TheNecronomicon Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

theNecronomicon :: AssetId -> TheNecronomicon
theNecronomicon uuid = TheNecronomicon
  $ (baseAttrs uuid "01009") { assetSlots = [HandSlot], assetHorror = Just 3 }

instance IsInvestigator investigator => HasModifiersFor env investigator TheNecronomicon where
  getModifiersFor _ i (TheNecronomicon a) =
    pure [ ForcedTokenChange Token.ElderSign Token.AutoFail | ownedBy a i ]

instance (IsInvestigator investigator) => HasActions env investigator TheNecronomicon where
  getActions i NonFast (TheNecronomicon a) | ownedBy a i = pure
    [ ActivateCardAbilityAction
        (getId () i)
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
