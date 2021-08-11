module Arkham.Types.Asset.Cards.MedicalTexts
  ( MedicalTexts(..)
  , medicalTexts
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetCard MedicalTexts
medicalTexts = hand MedicalTexts Cards.medicalTexts

instance HasModifiersFor env MedicalTexts

instance HasActions MedicalTexts where
  getActions (MedicalTexts a) =
    [restrictedAbility a 1 OwnsThis (ActionAbility Nothing $ ActionCost 1)]

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
