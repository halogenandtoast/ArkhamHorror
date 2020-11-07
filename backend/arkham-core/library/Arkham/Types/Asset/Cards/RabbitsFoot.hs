{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.RabbitsFoot where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype RabbitsFoot = RabbitsFoot Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rabbitsFoot :: AssetId -> RabbitsFoot
rabbitsFoot uuid =
  RabbitsFoot $ baseAttrs uuid "01075" $ slots .= [AccessorySlot]

instance HasModifiersFor env RabbitsFoot where
  getModifiersFor _ _ _ = pure []

instance HasActions env RabbitsFoot where
  getActions iid (AfterFailSkillTest You n) (RabbitsFoot a) | ownedBy a iid =
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ReactionAbility (AfterFailSkillTest You n))
          )
      | not (assetExhausted a)
      ]
  getActions i window (RabbitsFoot x) = getActions i window x

instance AssetRunner env => RunMessage env RabbitsFoot where
  runMessage msg (RabbitsFoot attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ RabbitsFoot $ attrs & exhausted .~ True
    _ -> RabbitsFoot <$> runMessage msg attrs
