module Arkham.Types.Asset.Cards.MedicalTexts
  ( MedicalTexts(..)
  , medicalTexts
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetCard MedicalTexts
medicalTexts = hand MedicalTexts Cards.medicalTexts

instance HasAbilities MedicalTexts where
  getAbilities (MedicalTexts a) =
    [restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ ActionCost 1]

instance AssetRunner env => RunMessage env MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationInvestigatorIds <- getSetList locationId
      push
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [ BeginSkillTest
                  iid
                  source
                  (InvestigatorTarget iid')
                  Nothing
                  SkillIntellect
                  2
              ]
          | iid' <- locationInvestigatorIds
          ]
        )
      MedicalTexts <$> runMessage msg attrs
    PassedSkillTest _ _ source (SkillTestInitiatorTarget target@(InvestigatorTarget _)) _ _
      | isSource attrs source
      -> a <$ push (HealDamage target 1)
    FailedSkillTest _ _ source (SkillTestInitiatorTarget (InvestigatorTarget iid)) _ _
      | isSource attrs source
      -> a <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> MedicalTexts <$> runMessage msg attrs
