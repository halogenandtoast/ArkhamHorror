{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MedicalTexts where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype MedicalTexts = MedicalTexts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

medicalTexts :: AssetId -> MedicalTexts
medicalTexts uuid =
  MedicalTexts $ (baseAttrs uuid "01035") { assetSlots = [HandSlot] }

instance (IsInvestigator investigator) => HasActions env investigator MedicalTexts where
  getActions i NonFast (MedicalTexts Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env MedicalTexts where
  runMessage msg (MedicalTexts attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      locationId <- asks (getId @LocationId (getInvestigator attrs))
      locationInvestigatorIds <- HashSet.toList <$> asks (getSet locationId)
      unshiftMessage
        (Ask iid $ ChooseOne
          [ BeginSkillTest
              iid
              (AssetSource aid)
              Nothing
              SkillIntellect
              2
              [HealDamage (InvestigatorTarget iid') 1]
              [InvestigatorAssignDamage iid' (AssetSource aid) 1 0]
              []
              mempty
          | iid' <- locationInvestigatorIds
          ]
        )
      MedicalTexts <$> runMessage msg attrs
    _ -> MedicalTexts <$> runMessage msg attrs
