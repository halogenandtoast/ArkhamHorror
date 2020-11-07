{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MedicalTexts where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype MedicalTexts = MedicalTexts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

medicalTexts :: AssetId -> MedicalTexts
medicalTexts uuid = MedicalTexts $ baseAttrs uuid "01035" $ slots .= [HandSlot]

instance HasModifiersFor env MedicalTexts where
  getModifiersFor _ _ _ = pure []

instance HasActions env MedicalTexts where
  getActions iid NonFast (MedicalTexts a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env MedicalTexts where
  runMessage msg (MedicalTexts attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId (getInvestigator attrs)
      locationInvestigatorIds <- asks $ setToList . getSet locationId
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
              [HealDamage (InvestigatorTarget iid') 1]
              [InvestigatorAssignDamage iid' (toSource attrs) 1 0]
              []
              mempty
          | iid' <- locationInvestigatorIds
          ]
        )
      MedicalTexts <$> runMessage msg attrs
    _ -> MedicalTexts <$> runMessage msg attrs
