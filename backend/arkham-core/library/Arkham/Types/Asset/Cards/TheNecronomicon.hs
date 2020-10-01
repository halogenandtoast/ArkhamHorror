{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.TheNecronomicon where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
import Arkham.Types.Window
import ClassyPrelude
import Safe (fromJustNote)

newtype TheNecronomicon = TheNecronomicon Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

theNecronomicon :: AssetId -> TheNecronomicon
theNecronomicon uuid = TheNecronomicon
  $ (baseAttrs uuid "01009") { assetSlots = [HandSlot], assetHorror = Just 3 }

instance IsInvestigator investigator => HasModifiersFor env investigator TheNecronomicon where
  getModifiersFor i (TheNecronomicon Attrs {..}) = pure
    [ ForcedTokenChange Token.ElderSign Token.AutoFail
    | Just (getId () i) == assetInvestigator
    ]


instance (IsInvestigator investigator) => HasActions env investigator TheNecronomicon where
  getActions i NonFast (TheNecronomicon Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      | fromJustNote "Must be set" assetHorror > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      unshiftMessage $ InvestigatorDamage iid (AssetSource aid) 0 1
      if fromJustNote "Must be set" assetHorror == 1
        then a <$ unshiftMessage (Discard (AssetTarget aid))
        else pure $ TheNecronomicon
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror })
    _ -> TheNecronomicon <$> runMessage msg attrs
