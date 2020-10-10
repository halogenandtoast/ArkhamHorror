{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ZoeysCross where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype ZoeysCross = ZoeysCross Attrs
  deriving newtype (Show, ToJSON, FromJSON)

zoeysCross :: AssetId -> ZoeysCross
zoeysCross uuid =
  ZoeysCross $ (baseAttrs uuid "02006") { assetSlots = [AccessorySlot] }

instance HasModifiersFor env investigator ZoeysCross where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> EnemyId -> Ability
ability attrs eid = (mkAbility
                      (toSource attrs)
                      1
                      (ReactionAbility (AfterEnemyEngageInvestigator You eid))
                    )
  { abilityMetadata = Just (TargetMetadata (EnemyTarget eid))
  }

instance (ActionRunner env investigator) => HasActions env investigator ZoeysCross where
  getActions i (AfterEnemyEngageInvestigator You eid) (ZoeysCross a@Attrs {..})
    | ownedBy a i = do
      let ability' = (getId () i, ability a eid)
      unused <- asks $ notElem ability' . map unUsedAbility . getList ()
      pure
        [ uncurry ActivateCardAbilityAction ability'
        | unused && resourceCount i > 0
        ]
  getActions i window (ZoeysCross x) = getActions i window x

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility iid source (Just (TargetMetadata (EnemyTarget eid))) 1
      | isSource attrs source -> a
      <$ unshiftMessage (EnemyDamage eid iid source 1)
    _ -> ZoeysCross <$> runMessage msg attrs
