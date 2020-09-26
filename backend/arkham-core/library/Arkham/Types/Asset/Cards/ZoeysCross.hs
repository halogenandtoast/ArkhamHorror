{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ZoeysCross where

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

newtype ZoeysCross = ZoeysCross Attrs
  deriving newtype (Show, ToJSON, FromJSON)

zoeysCross :: AssetId -> ZoeysCross
zoeysCross uuid =
  ZoeysCross $ (baseAttrs uuid "02006") { assetSlots = [AccessorySlot] }

instance (ActionRunner env investigator) => HasActions env investigator ZoeysCross where
  getActions i (AfterEnemyEngageInvestigator You eid) (ZoeysCross Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      let
        ability = (mkAbility
                    (AssetSource assetId)
                    1
                    (ReactionAbility (AfterEnemyEngageInvestigator You eid))
                  )
          { abilityMetadata = Just (TargetMetadata (EnemyTarget eid))
          }
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction (getId () i) ability
        | (getId () i, ability) `notElem` usedAbilities && resourceCount i > 0
        ]
  getActions i window (ZoeysCross x) = getActions i window x

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) (Just (TargetMetadata (EnemyTarget eid))) 1
      | aid == assetId
      -> a <$ unshiftMessage (EnemyDamage eid iid (AssetSource assetId) 1)
    _ -> ZoeysCross <$> runMessage msg attrs
