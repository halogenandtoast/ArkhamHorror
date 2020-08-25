{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.RabbitsFoot where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import ClassyPrelude
import Lens.Micro


newtype RabbitsFoot = RabbitsFoot Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rabbitsFoot :: AssetId -> RabbitsFoot
rabbitsFoot uuid =
  RabbitsFoot $ (baseAttrs uuid "01075") { assetSlots = [AccessorySlot] }

instance (IsInvestigator investigator) => HasActions env investigator RabbitsFoot where
  getActions i (AfterFailSkillTest You) (RabbitsFoot Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility
            (AssetSource assetId)
            1
            (ReactionAbility (AfterFailSkillTest You))
          )
      ]
  getActions i window (RabbitsFoot x) = getActions i window x

instance (AssetRunner env) => RunMessage env RabbitsFoot where
  runMessage msg (RabbitsFoot attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ RabbitsFoot $ attrs & exhausted .~ True
    _ -> RabbitsFoot <$> runMessage msg attrs
