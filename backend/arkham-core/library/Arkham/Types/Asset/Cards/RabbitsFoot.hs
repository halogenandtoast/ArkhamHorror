{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.RabbitsFoot where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype RabbitsFoot = RabbitsFoot Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rabbitsFoot :: AssetId -> RabbitsFoot
rabbitsFoot uuid =
  RabbitsFoot $ (baseAttrs uuid "01075") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env investigator RabbitsFoot where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator RabbitsFoot where
  getActions i (AfterFailSkillTest You n) (RabbitsFoot a) | ownedBy a i = pure
    [ ActivateCardAbilityAction
        (getId () i)
        (mkAbility (toSource a) 1 (ReactionAbility (AfterFailSkillTest You n)))
    | not (assetExhausted a)
    ]
  getActions i window (RabbitsFoot x) = getActions i window x

instance (AssetRunner env) => RunMessage env RabbitsFoot where
  runMessage msg (RabbitsFoot attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ RabbitsFoot $ attrs & exhausted .~ True
    _ -> RabbitsFoot <$> runMessage msg attrs
