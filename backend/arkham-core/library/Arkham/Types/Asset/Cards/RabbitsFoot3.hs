{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.RabbitsFoot3 where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro


newtype RabbitsFoot3 = RabbitsFoot3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rabbitsFoot3 :: AssetId -> RabbitsFoot3
rabbitsFoot3 uuid =
  RabbitsFoot3 $ (baseAttrs uuid "50010") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env investigator RabbitsFoot3 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator RabbitsFoot3 where
  getActions i (AfterFailSkillTest You n) (RabbitsFoot3 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          ((mkAbility
             (AssetSource assetId)
             1
             (ReactionAbility (AfterFailSkillTest You n))
           )
            { abilityMetadata = Just (IntMetadata n)
            }
          )
      | not assetExhausted
      ]
  getActions i window (RabbitsFoot3 x) = getActions i window x

instance (AssetRunner env) => RunMessage env RabbitsFoot3 where
  runMessage msg (RabbitsFoot3 attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) (Just (IntMetadata x)) 1
      | aid == assetId -> do
        unshiftMessage
          (SearchTopOfDeck iid (InvestigatorTarget iid) x mempty ShuffleBackIn)
        pure $ RabbitsFoot3 $ attrs & exhausted .~ True
    _ -> RabbitsFoot3 <$> runMessage msg attrs
