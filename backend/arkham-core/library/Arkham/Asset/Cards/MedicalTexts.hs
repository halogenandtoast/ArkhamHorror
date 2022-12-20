module Arkham.Asset.Cards.MedicalTexts
  ( MedicalTexts(..)
  , medicalTexts
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetCard MedicalTexts
medicalTexts = asset MedicalTexts Cards.medicalTexts

instance HasAbilities MedicalTexts where
  getAbilities (MedicalTexts a) =
    [restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let controllerId = getController attrs
      locationInvestigatorIds <- selectList $ colocatedWith controllerId
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
      -> a <$ push (HealDamage target (toSource attrs) 1)
    FailedSkillTest _ _ source (SkillTestInitiatorTarget (InvestigatorTarget iid)) _ _
      | isSource attrs source
      -> a <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> MedicalTexts <$> runMessage msg attrs
