{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PoliceBadge2 where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude

newtype PoliceBadge2 = PoliceBadge2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

policeBadge2 :: AssetId -> PoliceBadge2
policeBadge2 uuid =
  PoliceBadge2 $ (baseAttrs uuid "01027") { assetSlots = [AccessorySlot] }

instance IsInvestigator investigator => HasModifiersFor env investigator PoliceBadge2 where
  getModifiersFor _ i (PoliceBadge2 Attrs {..}) =
    pure
      [ SkillModifier SkillWillpower 1
      | Just (getId () i) == assetInvestigator
      ]

instance (ActionRunner env investigator) => HasActions env investigator PoliceBadge2 where
  getActions i (DuringTurn InvestigatorAtYourLocation) (PoliceBadge2 Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      pure
        [ ActivateCardAbilityAction
            (getId () i)
            (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (AssetSource aid) _ 1 | aid == assetId -> do
      activeInvestigatorId <- unActiveInvestigatorId <$> asks (getId ())
      a <$ unshiftMessages
        [ Discard (AssetTarget aid)
        , GainActions activeInvestigatorId (AssetSource aid) 2
        ]
    _ -> PoliceBadge2 <$> runMessage msg attrs
