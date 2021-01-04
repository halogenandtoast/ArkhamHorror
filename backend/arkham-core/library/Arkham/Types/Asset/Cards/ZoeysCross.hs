{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.ZoeysCross
  ( ZoeysCross(..)
  , zoeysCross
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype ZoeysCross = ZoeysCross Attrs
  deriving newtype (Show, ToJSON, FromJSON)

zoeysCross :: AssetId -> ZoeysCross
zoeysCross uuid =
  ZoeysCross $ (baseAttrs uuid "02006") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env ZoeysCross where
  getModifiersFor = noModifiersFor

ability :: Attrs -> EnemyId -> Ability
ability attrs eid = base
  { abilityMetadata = Just (TargetMetadata (EnemyTarget eid))
  }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ReactionAbility $ Costs [ExhaustCost (toTarget attrs), ResourceCost 1])

instance HasActions env ZoeysCross where
  getActions iid (AfterEnemyEngageInvestigator You eid) (ZoeysCross a@Attrs {..})
    | ownedBy a iid
    = pure [ActivateCardAbilityAction iid (ability a eid)]
  getActions i window (ZoeysCross x) = getActions i window x

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility iid source (Just (TargetMetadata (EnemyTarget eid))) 1
      | isSource attrs source -> a
      <$ unshiftMessage (EnemyDamage eid iid source 1)
    _ -> ZoeysCross <$> runMessage msg attrs
