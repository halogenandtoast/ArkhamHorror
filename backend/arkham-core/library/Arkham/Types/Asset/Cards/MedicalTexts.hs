{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MedicalTexts where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype MedicalTexts = MedicalTexts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

medicalTexts :: AssetId -> MedicalTexts
medicalTexts uuid =
  MedicalTexts $ (baseAttrs uuid "01035") { assetSlots = [HandSlot] }

instance HasModifiersFor env MedicalTexts where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MedicalTexts where
  getActions iid NonFast (MedicalTexts a) | ownedBy a iid = do
    canAffordActions <- getCanAffordCost
      iid
      (toSource a)
      (ActionCost 1 Nothing (assetTraits a))
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
      | canAffordActions
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationInvestigatorIds <- getSetList locationId
      unshiftMessage
        (chooseOne
          iid
          [ BeginSkillTest
              iid
              source
              (InvestigatorTarget iid')
              Nothing
              SkillIntellect
              2
          | iid' <- locationInvestigatorIds
          ]
        )
      MedicalTexts <$> runMessage msg attrs
    PassedSkillTest _ _ source target _ | isSource attrs source ->
      a <$ unshiftMessage (HealDamage target 1)
    FailedSkillTest _ _ source (InvestigatorTarget iid) _
      | isSource attrs source -> a
      <$ unshiftMessage (InvestigatorAssignDamage iid source 1 0)
    _ -> MedicalTexts <$> runMessage msg attrs
